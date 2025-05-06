;; title: VCC (Voting for Cooperatives & Communities)
;; version: 1.0
;; summary: A decentralized voting system for cooperatives and unions
;; description: This contract enables transparent and tamper-proof voting for cooperatives and unions,
;;              with support for weighted voting and proposal management.

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-EXISTS (err u101))
(define-constant ERR-NO-SUCH-PROPOSAL (err u102))
(define-constant ERR-VOTING-CLOSED (err u103))
(define-constant ERR-ALREADY-VOTED (err u104))
(define-constant ERR-INSUFFICIENT-VOTING-POWER (err u105))
(define-constant ERR-VOTING-STILL-ACTIVE (err u106))
(define-constant ERR-INVALID-VOTE (err u107))
(define-constant ERR-NOT-MEMBER (err u108))

;; Data variables
(define-data-var next-proposal-id uint u1)
(define-data-var membership-fee uint u100) ;; in microSTX

;; Data maps
;; Map to store member information
(define-map members 
  { address: principal } 
  { 
    voting-power: uint,
    is-active: bool,
    joined-at: uint
  }
)

;; Map to store proposal information
(define-map proposals
  { proposal-id: uint }
  {
    title: (string-ascii 100),
    description: (string-utf8 500),
    proposer: principal,
    created-at: uint,
    expires-at: uint,
    yes-votes: uint,
    no-votes: uint,
    abstain-votes: uint,
    status: (string-ascii 20), ;; "active", "passed", "rejected", "expired"
    min-voting-power: uint
  }
)

;; Map to track who has voted on which proposal
(define-map votes
  { proposal-id: uint, voter: principal }
  { 
    vote: (string-ascii 10), ;; "yes", "no", "abstain"
    weight: uint,
    voted-at: uint
  }
)

;; Public functions

;; Join as a member by paying the membership fee
(define-public (join-as-member)
  (let
    (
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (is-eq (stx-transfer? (var-get membership-fee) tx-sender (as-contract tx-sender)) (ok true)) (err u109))
    (ok (map-set members 
      { address: tx-sender } 
      { 
        voting-power: u1, ;; Default voting power
        is-active: true,
        joined-at: current-time
      }
    ))
  )
)

;; Create a new proposal
(define-public (create-proposal (title (string-ascii 100)) (description (string-utf8 500)) (duration uint) (min-voting-power uint))
  (let
    (
      (proposal-id (var-get next-proposal-id))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (expiration-time (+ current-time (* duration u60 u60 u24))) ;; Convert days to seconds
      (member-info (unwrap! (map-get? members { address: tx-sender }) ERR-NOT-MEMBER))
    )
    ;; Check if member is active
    (asserts! (get is-active member-info) ERR-NOT-AUTHORIZED)
    
    ;; Create the proposal
    (map-set proposals
      { proposal-id: proposal-id }
      {
        title: title,
        description: description,
        proposer: tx-sender,
        created-at: current-time,
        expires-at: expiration-time,
        yes-votes: u0,
        no-votes: u0,
        abstain-votes: u0,
        status: "active",
        min-voting-power: min-voting-power
      }
    )
    
    ;; Increment the proposal ID counter
    (var-set next-proposal-id (+ proposal-id u1))
    
    (ok proposal-id)
  )
)

;; Cast a vote on a proposal
(define-public (vote (proposal-id uint) (vote-value (string-ascii 10)))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NO-SUCH-PROPOSAL))
      (member-info (unwrap! (map-get? members { address: tx-sender }) ERR-NOT-MEMBER))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (voting-power (get voting-power member-info))
    )
    ;; Check if member is active
    (asserts! (get is-active member-info) ERR-NOT-AUTHORIZED)
    
    ;; Check if voting is still open
    (asserts! (< current-time (get expires-at proposal)) ERR-VOTING-CLOSED)
    
    ;; Check if member has sufficient voting power
    (asserts! (>= voting-power (get min-voting-power proposal)) ERR-INSUFFICIENT-VOTING-POWER)
    
    ;; Check if member has already voted
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    
    ;; Check if vote is valid
    (asserts! (or (is-eq vote-value "yes") (is-eq vote-value "no") (is-eq vote-value "abstain")) ERR-INVALID-VOTE)
    
    ;; Record the vote
    (map-set votes
      { proposal-id: proposal-id, voter: tx-sender }
      {
        vote: vote-value,
        weight: voting-power,
        voted-at: current-time
      }
    )
    
    ;; Update vote counts
    (if (is-eq vote-value "yes")
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { yes-votes: (+ (get yes-votes proposal) voting-power) })
      )
      (if (is-eq vote-value "no")
        (map-set proposals
          { proposal-id: proposal-id }
          (merge proposal { no-votes: (+ (get no-votes proposal) voting-power) })
        )
        (map-set proposals
          { proposal-id: proposal-id }
          (merge proposal { abstain-votes: (+ (get abstain-votes proposal) voting-power) })
        )
      )
    )
    
    (ok true)
  )
)

;; Finalize a proposal after voting period ends
(define-public (finalize-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NO-SUCH-PROPOSAL))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (yes-votes (get yes-votes proposal))
      (no-votes (get no-votes proposal))
      (total-votes (+ yes-votes no-votes))
      (new-status (if (> yes-votes no-votes) "passed" "rejected"))
    )
    ;; Check if voting period has ended
    (asserts! (>= current-time (get expires-at proposal)) ERR-VOTING-STILL-ACTIVE)
    
    ;; Check if proposal is still active
    (asserts! (is-eq (get status proposal) "active") (err u110))
    
    ;; Update proposal status
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { status: new-status })
    )
    
    (ok new-status)
  )
)

;; Admin function to update a member's voting power
(define-public (update-voting-power (member principal) (new-voting-power uint))
  (begin
    ;; Only contract owner can update voting power
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Check if the member exists
    (match (map-get? members { address: member })
      member-info (ok (map-set members
        { address: member }
        (merge member-info { voting-power: new-voting-power })
      ))
      ERR-NOT-MEMBER
    )
  )
)

;; Admin function to deactivate a member
(define-public (deactivate-member (member principal))
  (begin
    ;; Only contract owner can deactivate members
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Check if the member exists
    (match (map-get? members { address: member })
      member-info (ok (map-set members
        { address: member }
        (merge member-info { is-active: false })
      ))
      ERR-NOT-MEMBER
    )
  )
)

;; Admin function to reactivate a member
(define-public (reactivate-member (member principal))
  (begin
    ;; Only contract owner can reactivate members
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    
    ;; Check if the member exists
    (match (map-get? members { address: member })
      member-info (ok (map-set members
        { address: member }
        (merge member-info { is-active: true })
      ))
      ERR-NOT-MEMBER
    )
  )
)

;; Admin function to update membership fee
(define-public (update-membership-fee (new-fee uint))
  (begin
    ;; Only contract owner can update the fee
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (var-set membership-fee new-fee))
  )
)

;; Read-only functions

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

;; Get member details
(define-read-only (get-member (address principal))
  (map-get? members { address: address })
)

;; Get vote details
(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

;; Check if a member has voted on a proposal
(define-read-only (has-voted (proposal-id uint) (voter principal))
  (is-some (map-get? votes { proposal-id: proposal-id, voter: voter }))
)

;; Get current membership fee
(define-read-only (get-membership-fee)
  (var-get membership-fee)
)

;; Get total number of proposals
(define-read-only (get-proposal-count)
  (- (var-get next-proposal-id) u1)
)

(define-map vote-delegates
  { delegator: principal }
  { 
    delegate: principal,
    delegated-at: uint
  }
)

(define-public (delegate-vote-power (delegate-to principal))
  (let
    (
      (delegator-info (unwrap! (map-get? members { address: tx-sender }) ERR-NOT-MEMBER))
      (delegate-info (unwrap! (map-get? members { address: delegate-to }) ERR-NOT-MEMBER))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (asserts! (get is-active delegator-info) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active delegate-info) ERR-NOT-AUTHORIZED)
    (ok (map-set vote-delegates
      { delegator: tx-sender }
      {
        delegate: delegate-to,
        delegated-at: current-time
      }
    ))
  )
)

(define-public (revoke-delegation)
  (ok (map-delete vote-delegates { delegator: tx-sender }))
)

(define-read-only (get-delegate (delegator principal))
  (map-get? vote-delegates { delegator: delegator })
)


(define-map proposal-categories
  { category-id: uint }
  { name: (string-ascii 50) }
)

(define-map proposal-tags
  { proposal-id: uint }
  { categories: (list 10 uint) }
)

(define-data-var next-category-id uint u1)

(define-public (create-category (name (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (let ((category-id (var-get next-category-id)))
      (map-set proposal-categories
        { category-id: category-id }
        { name: name }
      )
      (var-set next-category-id (+ category-id u1))
      (ok category-id)
    )
  )
)

(define-public (add-proposal-categories (proposal-id uint) (categories (list 10 uint)))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NO-SUCH-PROPOSAL))
    )
    (asserts! (is-eq tx-sender (get proposer proposal)) ERR-NOT-AUTHORIZED)
    (ok (map-set proposal-tags
      { proposal-id: proposal-id }
      { categories: categories }
    ))
  )
)

(define-read-only (get-proposal-categories (proposal-id uint))
  (map-get? proposal-tags { proposal-id: proposal-id })
)