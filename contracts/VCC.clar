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


(define-constant ERR-AMENDMENT-EXISTS (err u115))
(define-constant ERR-NO-SUCH-AMENDMENT (err u116))
(define-constant ERR-AMENDMENT-ALREADY-APPLIED (err u117))
(define-constant ERR-CANNOT-AMEND-FINALIZED (err u118))

(define-data-var next-amendment-id uint u1)

(define-map proposal-amendments
  { amendment-id: uint }
  {
    proposal-id: uint,
    new-title: (optional (string-ascii 100)),
    new-description: (optional (string-utf8 500)),
    new-duration: (optional uint),
    proposer: principal,
    created-at: uint,
    votes-for: uint,
    votes-against: uint,
    status: (string-ascii 20),
    required-approvals: uint
  }
)

(define-map amendment-votes
  { amendment-id: uint, voter: principal }
  {
    vote: (string-ascii 10),
    voted-at: uint
  }
)

(define-public (create-amendment 
  (proposal-id uint) 
  (new-title (optional (string-ascii 100))) 
  (new-description (optional (string-utf8 500))) 
  (new-duration (optional uint)))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NO-SUCH-PROPOSAL))
      (member-info (unwrap! (map-get? members { address: tx-sender }) ERR-NOT-MEMBER))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (amendment-id (var-get next-amendment-id))
      (total-votes (+ (get yes-votes proposal) (get no-votes proposal) (get abstain-votes proposal)))
      (required-approvals (/ total-votes u2))
    )
    (asserts! (get is-active member-info) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status proposal) "active") ERR-CANNOT-AMEND-FINALIZED)
    (asserts! (< current-time (get expires-at proposal)) ERR-VOTING-CLOSED)
    (asserts! (or (is-some new-title) (is-some new-description) (is-some new-duration)) ERR-EXECUTION-FAILED)
    
    (map-set proposal-amendments
      { amendment-id: amendment-id }
      {
        proposal-id: proposal-id,
        new-title: new-title,
        new-description: new-description,
        new-duration: new-duration,
        proposer: tx-sender,
        created-at: current-time,
        votes-for: u0,
        votes-against: u0,
        status: "pending",
        required-approvals: (if (> required-approvals u0) required-approvals u1)
      }
    )
    
    (var-set next-amendment-id (+ amendment-id u1))
    (ok amendment-id)
  )
)

(define-public (vote-on-amendment (amendment-id uint) (support bool))
  (let
    (
      (amendment (unwrap! (map-get? proposal-amendments { amendment-id: amendment-id }) ERR-NO-SUCH-AMENDMENT))
      (member-info (unwrap! (map-get? members { address: tx-sender }) ERR-NOT-MEMBER))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (vote-value (if support "for" "against"))
      (current-votes-for (get votes-for amendment))
      (current-votes-against (get votes-against amendment))
    )
    (asserts! (get is-active member-info) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status amendment) "pending") ERR-AMENDMENT-ALREADY-APPLIED)
    (asserts! (is-none (map-get? amendment-votes { amendment-id: amendment-id, voter: tx-sender })) ERR-ALREADY-VOTED)
    
    (map-set amendment-votes
      { amendment-id: amendment-id, voter: tx-sender }
      {
        vote: vote-value,
        voted-at: current-time
      }
    )
    
    (let
      (
        (new-votes-for (if support (+ current-votes-for u1) current-votes-for))
        (new-votes-against (if support current-votes-against (+ current-votes-against u1)))
        (updated-amendment (merge amendment { votes-for: new-votes-for, votes-against: new-votes-against }))
      )
      (if (>= new-votes-for (get required-approvals amendment))
        (begin
          (try! (apply-amendment amendment-id))
          (map-set proposal-amendments
            { amendment-id: amendment-id }
            (merge updated-amendment { status: "applied" })
          )
        )
        (map-set proposal-amendments
          { amendment-id: amendment-id }
          updated-amendment
        )
      )
    )
    
    (ok true)
  )
)

(define-private (apply-amendment (amendment-id uint))
  (let
    (
      (amendment (unwrap! (map-get? proposal-amendments { amendment-id: amendment-id }) ERR-NO-SUCH-AMENDMENT))
      (proposal-id (get proposal-id amendment))
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NO-SUCH-PROPOSAL))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
    )
    (let
      (
        (updated-proposal (merge proposal {
          title: (default-to (get title proposal) (get new-title amendment)),
          description: (default-to (get description proposal) (get new-description amendment)),
          expires-at: (match (get new-duration amendment)
            new-duration (+ current-time (* new-duration u60 u60 u24))
            (get expires-at proposal)
          )
        }))
      )
      (map-set proposals
        { proposal-id: proposal-id }
        updated-proposal
      )
      (ok true)
    )
  )
)

(define-read-only (get-amendment (amendment-id uint))
  (map-get? proposal-amendments { amendment-id: amendment-id })
)

(define-read-only (get-amendment-vote (amendment-id uint) (voter principal))
  (map-get? amendment-votes { amendment-id: amendment-id, voter: voter })
)

(define-read-only (get-amendment-count)
  (- (var-get next-amendment-id) u1)
)

(define-read-only (has-voted-on-amendment (amendment-id uint) (voter principal))
  (is-some (map-get? amendment-votes { amendment-id: amendment-id, voter: voter }))
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

(define-constant ERR-EXECUTION-FAILED (err u111))
(define-constant ERR-NOT-EXECUTABLE (err u112))
(define-constant ERR-ALREADY-EXECUTED (err u113))
(define-constant ERR-PROPOSAL-NOT-PASSED (err u114))

(define-map executable-proposals
  { proposal-id: uint }
  {
    action-type: (string-ascii 20),
    target-contract: (optional principal),
    function-name: (optional (string-ascii 50)),
    amount: (optional uint),
    recipient: (optional principal),
    parameter-name: (optional (string-ascii 50)),
    parameter-value: (optional uint),
    is-executed: bool
  }
)

(define-public (create-executable-proposal 
  (title (string-ascii 100)) 
  (description (string-utf8 500)) 
  (duration uint) 
  (min-voting-power uint)
  (action-type (string-ascii 20))
  (target-contract (optional principal))
  (function-name (optional (string-ascii 50)))
  (amount (optional uint))
  (recipient (optional principal))
  (parameter-name (optional (string-ascii 50)))
  (parameter-value (optional uint)))
  (let
    (
      (proposal-result (try! (create-proposal title description duration min-voting-power)))
    )
    (map-set executable-proposals
      { proposal-id: proposal-result }
      {
        action-type: action-type,
        target-contract: target-contract,
        function-name: function-name,
        amount: amount,
        recipient: recipient,
        parameter-name: parameter-name,
        parameter-value: parameter-value,
        is-executed: false
      }
    )
    (ok proposal-result)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NO-SUCH-PROPOSAL))
      (executable-info (unwrap! (map-get? executable-proposals { proposal-id: proposal-id }) ERR-NOT-EXECUTABLE))
    )
    (asserts! (is-eq (get status proposal) "passed") ERR-PROPOSAL-NOT-PASSED)
    (asserts! (not (get is-executed executable-info)) ERR-ALREADY-EXECUTED)
    
    (let ((execution-result 
      (if (is-eq (get action-type executable-info) "transfer")
        (execute-transfer executable-info)
        (if (is-eq (get action-type executable-info) "parameter")
          (execute-parameter-update executable-info)
          ERR-EXECUTION-FAILED
        )
      )))
      (match execution-result
        success (begin
          (map-set executable-proposals
            { proposal-id: proposal-id }
            (merge executable-info { is-executed: true })
          )
          (ok true)
        )
        error (err error)
      )
    )
  )
)

(define-private (execute-transfer (executable-info (tuple (action-type (string-ascii 20)) (target-contract (optional principal)) (function-name (optional (string-ascii 50))) (amount (optional uint)) (recipient (optional principal)) (parameter-name (optional (string-ascii 50))) (parameter-value (optional uint)) (is-executed bool))))
  (let
    (
      (transfer-amount (unwrap! (get amount executable-info) ERR-EXECUTION-FAILED))
      (transfer-recipient (unwrap! (get recipient executable-info) ERR-EXECUTION-FAILED))
    )
    (as-contract (stx-transfer? transfer-amount tx-sender transfer-recipient))
  )
)

(define-private (execute-parameter-update (executable-info (tuple (action-type (string-ascii 20)) (target-contract (optional principal)) (function-name (optional (string-ascii 50))) (amount (optional uint)) (recipient (optional principal)) (parameter-name (optional (string-ascii 50))) (parameter-value (optional uint)) (is-executed bool))))
  (let
    (
      (param-name (unwrap! (get parameter-name executable-info) ERR-EXECUTION-FAILED))
      (param-value (unwrap! (get parameter-value executable-info) ERR-EXECUTION-FAILED))
    )
    (if (is-eq param-name "membership-fee")
      (ok (var-set membership-fee param-value))
      ERR-EXECUTION-FAILED
    )
  )
)

(define-read-only (get-executable-proposal (proposal-id uint))
  (map-get? executable-proposals { proposal-id: proposal-id })
)

(define-read-only (is-proposal-executed (proposal-id uint))
  (match (map-get? executable-proposals { proposal-id: proposal-id })
    executable-info (get is-executed executable-info)
    false
  )
)

;; Proposal Sponsorship System Constants
(define-constant ERR-INSUFFICIENT-SPONSORSHIP (err u119))
(define-constant ERR-SPONSORSHIP-CLOSED (err u120))
(define-constant ERR-NOT-SPONSOR (err u121))
(define-constant ERR-REWARDS-CLAIMED (err u122))
(define-constant ERR-PROPOSAL-NOT-FINALIZED (err u123))

;; Sponsorship data variables
(define-data-var min-sponsorship-amount uint u50) ;; Minimum STX to sponsor
(define-data-var sponsor-reward-percentage uint u5) ;; 5% bonus for sponsors of passed proposals

;; Proposal sponsorship tracking
(define-map proposal-sponsorships
  { proposal-id: uint }
  {
    total-sponsored: uint,
    sponsor-count: uint,
    rewards-distributed: bool,
    target-amount: uint
  }
)

;; Individual sponsor contributions
(define-map sponsor-contributions
  { proposal-id: uint, sponsor: principal }
  {
    amount: uint,
    sponsored-at: uint,
    rewards-claimed: bool
  }
)

;; Sponsor reward pool tracking
(define-map sponsor-rewards
  { proposal-id: uint }
  {
    total-pool: uint,
    per-token-reward: uint
  }
)

;; Create sponsored proposal with funding target
(define-public (create-sponsored-proposal 
  (title (string-ascii 100)) 
  (description (string-utf8 500)) 
  (duration uint) 
  (min-voting-power uint)
  (funding-target uint)
  (initial-sponsorship uint))
  (let
    (
      (proposal-result (try! (create-proposal title description duration min-voting-power)))
      (member-info (unwrap! (map-get? members { address: tx-sender }) ERR-NOT-MEMBER))
    )
    ;; Validate minimum sponsorship
    (asserts! (>= initial-sponsorship (var-get min-sponsorship-amount)) ERR-INSUFFICIENT-SPONSORSHIP)
    (asserts! (>= funding-target initial-sponsorship) ERR-INSUFFICIENT-SPONSORSHIP)
    
    ;; Transfer initial sponsorship to contract
    (try! (stx-transfer? initial-sponsorship tx-sender (as-contract tx-sender)))
    
    ;; Initialize proposal sponsorship data
    (map-set proposal-sponsorships
      { proposal-id: proposal-result }
      {
        total-sponsored: initial-sponsorship,
        sponsor-count: u1,
        rewards-distributed: false,
        target-amount: funding-target
      }
    )
    
    ;; Record sponsor contribution
    (map-set sponsor-contributions
      { proposal-id: proposal-result, sponsor: tx-sender }
      {
        amount: initial-sponsorship,
        sponsored-at: (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))),
        rewards-claimed: false
      }
    )
    
    (ok proposal-result)
  )
)

;; Sponsor an existing proposal
(define-public (sponsor-proposal (proposal-id uint) (amount uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NO-SUCH-PROPOSAL))
      (sponsorship-info (unwrap! (map-get? proposal-sponsorships { proposal-id: proposal-id }) ERR-INSUFFICIENT-SPONSORSHIP))
      (member-info (unwrap! (map-get? members { address: tx-sender }) ERR-NOT-MEMBER))
      (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
      (existing-contribution (map-get? sponsor-contributions { proposal-id: proposal-id, sponsor: tx-sender }))
    )
    ;; Validate sponsorship conditions
    (asserts! (>= amount (var-get min-sponsorship-amount)) ERR-INSUFFICIENT-SPONSORSHIP)
    (asserts! (is-eq (get status proposal) "active") ERR-SPONSORSHIP-CLOSED)
    (asserts! (< current-time (get expires-at proposal)) ERR-VOTING-CLOSED)
    (asserts! (get is-active member-info) ERR-NOT-AUTHORIZED)
    
    ;; Transfer sponsorship amount to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update or create sponsor contribution
    (match existing-contribution
      contribution (map-set sponsor-contributions
        { proposal-id: proposal-id, sponsor: tx-sender }
        {
          amount: (+ (get amount contribution) amount),
          sponsored-at: (get sponsored-at contribution),
          rewards-claimed: false
        }
      )
      (begin
        (map-set sponsor-contributions
          { proposal-id: proposal-id, sponsor: tx-sender }
          {
            amount: amount,
            sponsored-at: current-time,
            rewards-claimed: false
          }
        )
        ;; Increment sponsor count for new sponsors
        (map-set proposal-sponsorships
          { proposal-id: proposal-id }
          (merge sponsorship-info { sponsor-count: (+ (get sponsor-count sponsorship-info) u1) })
        )
      )
    )
    
    ;; Update total sponsored amount
    (map-set proposal-sponsorships
      { proposal-id: proposal-id }
      (merge sponsorship-info { total-sponsored: (+ (get total-sponsored sponsorship-info) amount) })
    )
    
    (ok true)
  )
)

;; Withdraw sponsorship for failed or rejected proposals
(define-public (withdraw-sponsorship (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NO-SUCH-PROPOSAL))
      (contribution (unwrap! (map-get? sponsor-contributions { proposal-id: proposal-id, sponsor: tx-sender }) ERR-NOT-SPONSOR))
      (sponsorship-info (unwrap! (map-get? proposal-sponsorships { proposal-id: proposal-id }) ERR-INSUFFICIENT-SPONSORSHIP))
    )
    ;; Only allow withdrawal for rejected, expired, or failed proposals
    (asserts! (or (is-eq (get status proposal) "rejected") 
                  (is-eq (get status proposal) "expired") 
                  (is-eq (get status proposal) "failed")) ERR-SPONSORSHIP-CLOSED)
    (asserts! (not (get rewards-claimed contribution)) ERR-REWARDS-CLAIMED)
    
    ;; Mark as withdrawn
    (map-set sponsor-contributions
      { proposal-id: proposal-id, sponsor: tx-sender }
      (merge contribution { rewards-claimed: true })
    )
    
    ;; Return sponsorship amount to sponsor
    (as-contract (stx-transfer? (get amount contribution) tx-sender tx-sender))
  )
)

;; Distribute rewards to sponsors of passed proposals
(define-public (claim-sponsor-rewards (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NO-SUCH-PROPOSAL))
      (contribution (unwrap! (map-get? sponsor-contributions { proposal-id: proposal-id, sponsor: tx-sender }) ERR-NOT-SPONSOR))
      (sponsorship-info (unwrap! (map-get? proposal-sponsorships { proposal-id: proposal-id }) ERR-INSUFFICIENT-SPONSORSHIP))
    )
    ;; Validate claim conditions
    (asserts! (is-eq (get status proposal) "passed") ERR-PROPOSAL-NOT-FINALIZED)
    (asserts! (not (get rewards-claimed contribution)) ERR-REWARDS-CLAIMED)
    
    ;; Calculate rewards
    (let
      (
        (sponsor-amount (get amount contribution))
        (total-sponsored (get total-sponsored sponsorship-info))
        (reward-percentage (var-get sponsor-reward-percentage))
        (bonus-reward (/ (* sponsor-amount reward-percentage) u100))
        (total-return (+ sponsor-amount bonus-reward))
      )
      ;; Mark rewards as claimed
      (map-set sponsor-contributions
        { proposal-id: proposal-id, sponsor: tx-sender }
        (merge contribution { rewards-claimed: true })
      )
      
      ;; Transfer original sponsorship plus bonus
      (as-contract (stx-transfer? total-return tx-sender tx-sender))
    )
  )
)

;; Admin function to update minimum sponsorship amount
(define-public (update-min-sponsorship (new-amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (var-set min-sponsorship-amount new-amount))
  )
)

;; Admin function to update sponsor reward percentage
(define-public (update-sponsor-reward-percentage (new-percentage uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-percentage u20) ERR-EXECUTION-FAILED) ;; Max 20% reward
    (ok (var-set sponsor-reward-percentage new-percentage))
  )
)

;; Read-only functions for sponsorship data

;; Get proposal sponsorship details
(define-read-only (get-proposal-sponsorship (proposal-id uint))
  (map-get? proposal-sponsorships { proposal-id: proposal-id })
)

;; Get individual sponsor contribution
(define-read-only (get-sponsor-contribution (proposal-id uint) (sponsor principal))
  (map-get? sponsor-contributions { proposal-id: proposal-id, sponsor: sponsor })
)

;; Check if sponsorship target is met
(define-read-only (is-sponsorship-target-met (proposal-id uint))
  (match (map-get? proposal-sponsorships { proposal-id: proposal-id })
    sponsorship-info (>= (get total-sponsored sponsorship-info) (get target-amount sponsorship-info))
    false
  )
)

;; Get current minimum sponsorship amount
(define-read-only (get-min-sponsorship-amount)
  (var-get min-sponsorship-amount)
)

;; Get current sponsor reward percentage
(define-read-only (get-sponsor-reward-percentage)
  (var-get sponsor-reward-percentage)
)

;; Calculate potential rewards for a sponsor
(define-read-only (calculate-sponsor-rewards (proposal-id uint) (sponsor principal))
  (match (map-get? sponsor-contributions { proposal-id: proposal-id, sponsor: sponsor })
    contribution (let
      (
        (sponsor-amount (get amount contribution))
        (reward-percentage (var-get sponsor-reward-percentage))
        (bonus-reward (/ (* sponsor-amount reward-percentage) u100))
      )
      (ok (+ sponsor-amount bonus-reward))
    )
    ERR-NOT-SPONSOR
  )
)



