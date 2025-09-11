;; Member Reputation & Merit System for VCC
;; Tracks member contributions, voting accuracy, and reputation for enhanced governance

;; Error constants
(define-constant ERR-NOT-MEMBER (err u200))
(define-constant ERR-INVALID-MERIT-ACTION (err u201))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u202))
(define-constant ERR-BADGE-ALREADY-AWARDED (err u203))
(define-constant ERR-INVALID-TIER (err u204))
(define-constant ERR-PRIVILEGE-DENIED (err u205))
(define-constant ERR-REPUTATION-LOCKED (err u206))

;; Data variables
(define-data-var base-voting-accuracy-weight uint u100)
(define-data-var merit-points-per-vote uint u5)
(define-data-var merit-points-per-proposal uint u50)
(define-data-var reputation-calculation-period uint u4320) ;; ~30 days in blocks

;; Member reputation tracking
(define-map member-reputation
  { member: principal }
  {
    total-merit-points: uint,
    voting-accuracy-score: uint, ;; 0-1000 scale
    total-votes-cast: uint,
    accurate-votes: uint, ;; Votes that aligned with final outcome
    proposals-created: uint,
    successful-proposals: uint, ;; Proposals that passed
    reputation-tier: uint, ;; 1-5 scale
    reputation-score: uint, ;; Combined metric
    last-updated: uint,
    participation-streak: uint ;; Consecutive active voting periods
  }
)

;; Achievement badges system
(define-map member-badges
  { member: principal, badge-type: (string-ascii 30) }
  {
    earned-at: uint,
    description: (string-ascii 100),
    merit-bonus: uint ;; Additional merit points for badge
  }
)

;; Reputation tier definitions
(define-map reputation-tiers
  { tier: uint }
  {
    min-score: uint,
    max-score: uint,
    voting-weight-multiplier: uint, ;; Basis points (10000 = 1x)
    tier-name: (string-ascii 20),
    privileges: (list 5 (string-ascii 30)),
    min-accuracy-required: uint
  }
)

;; Merit action tracking
(define-map merit-actions
  { member: principal, action-id: uint }
  {
    action-type: (string-ascii 30), ;; "vote", "proposal", "amendment", "participation"
    merit-earned: uint,
    recorded-at: uint,
    proposal-involved: (optional uint)
  }
)

;; Governance privileges tracking
(define-map member-privileges
  { member: principal }
  {
    can-create-priority-proposals: bool,
    can-extend-voting-periods: bool,
    can-moderate-discussions: bool,
    privilege-expires-at: uint,
    earned-through-tier: uint
  }
)

;; Member participation tracking
(define-map participation-history
  { member: principal, period: uint }
  {
    votes-cast: uint,
    proposals-created: uint,
    amendments-proposed: uint,
    participation-score: uint,
    period-start: uint,
    period-end: uint
  }
)

;; Initialize reputation tier system
(define-private (initialize-reputation-tiers)
  (begin
    (map-set reputation-tiers { tier: u1 } 
      { min-score: u0, max-score: u200, voting-weight-multiplier: u10000, 
        tier-name: "Newcomer", privileges: (list), min-accuracy-required: u0 })
    (map-set reputation-tiers { tier: u2 } 
      { min-score: u201, max-score: u500, voting-weight-multiplier: u11000, 
        tier-name: "Active Member", privileges: (list "moderate-discussions"), min-accuracy-required: u600 })
    (map-set reputation-tiers { tier: u3 } 
      { min-score: u501, max-score: u800, voting-weight-multiplier: u12500, 
        tier-name: "Trusted Member", privileges: (list "moderate-discussions" "extend-voting"), min-accuracy-required: u700 })
    (map-set reputation-tiers { tier: u4 } 
      { min-score: u801, max-score: u1200, voting-weight-multiplier: u15000, 
        tier-name: "Senior Member", privileges: (list "moderate-discussions" "extend-voting" "priority-proposals"), min-accuracy-required: u750 })
    (map-set reputation-tiers { tier: u5 } 
      { min-score: u1201, max-score: u9999, voting-weight-multiplier: u20000, 
        tier-name: "Elder", privileges: (list "moderate-discussions" "extend-voting" "priority-proposals" "governance-admin"), min-accuracy-required: u800 })
    (ok true)
  )
)

;; Award merit points for member actions
(define-public (award-merit-points 
  (member principal) 
  (action-type (string-ascii 30)) 
  (points uint)
  (proposal-id (optional uint)))
  (let
    (
      (member-rep (default-to 
        { total-merit-points: u0, voting-accuracy-score: u500, total-votes-cast: u0, 
          accurate-votes: u0, proposals-created: u0, successful-proposals: u0, 
          reputation-tier: u1, reputation-score: u0, last-updated: u0, participation-streak: u0 }
        (map-get? member-reputation { member: member })))
      (action-id (+ (get total-votes-cast member-rep) (get proposals-created member-rep)))
    )
    ;; Validate member exists in main contract
    (asserts! (is-some (contract-call? .VCC get-member member)) ERR-NOT-MEMBER)
    
    ;; Record merit action
    (map-set merit-actions
      { member: member, action-id: action-id }
      {
        action-type: action-type,
        merit-earned: points,
        recorded-at: stacks-block-height,
        proposal-involved: proposal-id
      }
    )
    
    ;; Update member reputation
    (let
      (
        (new-total-points (+ (get total-merit-points member-rep) points))
        (updated-rep (merge member-rep { 
          total-merit-points: new-total-points,
          last-updated: stacks-block-height
        }))
      )
      (map-set member-reputation { member: member } updated-rep)
      (try! (recalculate-reputation-tier member))
      (ok points)
    )
  )
)

;; Update voting accuracy after proposal finalization
(define-public (update-voting-accuracy (member principal) (proposal-id uint) (voted-correctly bool))
  (let
    (
      (member-rep (unwrap! (map-get? member-reputation { member: member }) ERR-NOT-MEMBER))
      (new-total-votes (+ (get total-votes-cast member-rep) u1))
      (new-accurate-votes (if voted-correctly 
                            (+ (get accurate-votes member-rep) u1) 
                            (get accurate-votes member-rep)))
      (new-accuracy-score (if (> new-total-votes u0) 
                            (/ (* new-accurate-votes u1000) new-total-votes) 
                            u500))
    )
    (map-set member-reputation
      { member: member }
      (merge member-rep {
        total-votes-cast: new-total-votes,
        accurate-votes: new-accurate-votes,
        voting-accuracy-score: new-accuracy-score,
        last-updated: stacks-block-height
      })
    )
    (try! (recalculate-reputation-tier member))
    (try! (check-and-award-badges member))
    (ok new-accuracy-score)
  )
)

;; Award achievement badges to members
(define-public (award-badge (member principal) (badge-type (string-ascii 30)) (description (string-ascii 100)) (merit-bonus uint))
  (let
    (
      (existing-badge (map-get? member-badges { member: member, badge-type: badge-type }))
    )
    (asserts! (is-none existing-badge) ERR-BADGE-ALREADY-AWARDED)
    (asserts! (is-some (contract-call? .VCC get-member member)) ERR-NOT-MEMBER)
    
    (map-set member-badges
      { member: member, badge-type: badge-type }
      {
        earned-at: stacks-block-height,
        description: description,
        merit-bonus: merit-bonus
      }
    )
    
    ;; Award bonus merit points
    (try! (award-merit-points member "badge-earned" merit-bonus none))
    (ok true)
  )
)

;; Private helper functions
(define-private (recalculate-reputation-tier (member principal))
  (let
    (
      (member-rep (unwrap! (map-get? member-reputation { member: member }) ERR-NOT-MEMBER))
      (reputation-score (calculate-reputation-score member-rep))
      (new-tier (determine-reputation-tier reputation-score))
      (privilege-duration u4320) ;; 30 days
    )
    (map-set member-reputation
      { member: member }
      (merge member-rep { 
        reputation-score: reputation-score,
        reputation-tier: new-tier
      })
    )
    ;; Update privileges directly
    (map-set member-privileges
      { member: member }
      {
        can-create-priority-proposals: (>= new-tier u4),
        can-extend-voting-periods: (>= new-tier u3),
        can-moderate-discussions: (>= new-tier u2),
        privilege-expires-at: (+ stacks-block-height privilege-duration),
        earned-through-tier: new-tier
      }
    )
    (ok new-tier)
  )
)

(define-private (calculate-reputation-score (member-rep (tuple (total-merit-points uint) (voting-accuracy-score uint) (total-votes-cast uint) (accurate-votes uint) (proposals-created uint) (successful-proposals uint) (reputation-tier uint) (reputation-score uint) (last-updated uint) (participation-streak uint))))
  (let
    (
      (merit-component (/ (get total-merit-points member-rep) u2))
      (accuracy-component (/ (get voting-accuracy-score member-rep) u2))
      (participation-component (* (get participation-streak member-rep) u10))
      (proposal-success-bonus (if (> (get proposals-created member-rep) u0)
                                 (* (/ (* (get successful-proposals member-rep) u100) 
                                      (get proposals-created member-rep)) u2)
                                 u0))
    )
    (+ merit-component accuracy-component participation-component proposal-success-bonus)
  )
)

(define-private (determine-reputation-tier (reputation-score uint))
  (if (>= reputation-score u1201) u5
    (if (>= reputation-score u801) u4
      (if (>= reputation-score u501) u3
        (if (>= reputation-score u201) u2
          u1))))
)

(define-private (update-member-privileges (member principal) (tier uint))
  (let
    (
      (tier-info (unwrap-panic (map-get? reputation-tiers { tier: tier })))
      (privilege-duration u4320) ;; 30 days
    )
    (map-set member-privileges
      { member: member }
      {
        can-create-priority-proposals: (>= tier u4),
        can-extend-voting-periods: (>= tier u3),
        can-moderate-discussions: (>= tier u2),
        privilege-expires-at: (+ stacks-block-height privilege-duration),
        earned-through-tier: tier
      }
    )
    (ok true)
  )
)

(define-private (check-and-award-badges (member principal))
  (let
    (
      (member-rep (unwrap! (map-get? member-reputation { member: member }) ERR-NOT-MEMBER))
      (total-votes (get total-votes-cast member-rep))
      (accuracy (get voting-accuracy-score member-rep))
      (proposals (get proposals-created member-rep))
    )
    ;; Check accuracy badge without recursive call
    (if (and (>= accuracy u900) (>= total-votes u10) (is-none (map-get? member-badges { member: member, badge-type: "accuracy-expert" })))
      (map-set member-badges
        { member: member, badge-type: "accuracy-expert" }
        {
          earned-at: stacks-block-height,
          description: "Maintains 90%+ voting accuracy",
          merit-bonus: u100
        }
      )
      true
    )
    
    ;; Check participation badge without recursive call
    (if (and (>= total-votes u50) (is-none (map-get? member-badges { member: member, badge-type: "active-voter" })))
      (map-set member-badges
        { member: member, badge-type: "active-voter" }
        {
          earned-at: stacks-block-height,
          description: "Cast 50+ votes in community decisions",
          merit-bonus: u75
        }
      )
      true
    )
    
    ;; Check proposal creation badge without recursive call
    (if (and (>= proposals u5) (is-none (map-get? member-badges { member: member, badge-type: "proposal-creator" })))
      (map-set member-badges
        { member: member, badge-type: "proposal-creator" }
        {
          earned-at: stacks-block-height,
          description: "Created 5+ community proposals",
          merit-bonus: u150
        }
      )
      true
    )
    
    (ok true)
  )
)

;; Calculate effective voting weight with reputation multiplier
(define-public (get-effective-voting-weight (member principal))
  (let
    (
      (base-member (unwrap! (contract-call? .VCC get-member member) ERR-NOT-MEMBER))
      (member-rep (map-get? member-reputation { member: member }))
      (base-weight (get voting-power base-member))
    )
    (match member-rep
      rep-data (let
        (
          (tier (get reputation-tier rep-data))
          (tier-info (unwrap-panic (map-get? reputation-tiers { tier: tier })))
          (multiplier (get voting-weight-multiplier tier-info))
          (effective-weight (/ (* base-weight multiplier) u10000))
        )
        (ok effective-weight)
      )
      (ok base-weight) ;; Default to base weight if no reputation data
    )
  )
)

;; Update participation streak
(define-public (update-participation-streak (member principal) (participated bool))
  (let
    (
      (member-rep (default-to 
        { total-merit-points: u0, voting-accuracy-score: u500, total-votes-cast: u0, 
          accurate-votes: u0, proposals-created: u0, successful-proposals: u0, 
          reputation-tier: u1, reputation-score: u0, last-updated: u0, participation-streak: u0 }
        (map-get? member-reputation { member: member })))
      (current-streak (get participation-streak member-rep))
      (new-streak (if participated (+ current-streak u1) u0))
    )
    (map-set member-reputation
      { member: member }
      (merge member-rep { 
        participation-streak: new-streak,
        last-updated: stacks-block-height
      })
    )
    (ok new-streak)
  )
)

;; Check if member has specific privilege
(define-public (has-privilege (member principal) (privilege-type (string-ascii 30)))
  (let
    (
      (privileges (map-get? member-privileges { member: member }))
    )
    (match privileges
      priv-data (ok (and 
        (< stacks-block-height (get privilege-expires-at priv-data))
        (if (is-eq privilege-type "priority-proposals") 
          (get can-create-priority-proposals priv-data)
          (if (is-eq privilege-type "extend-voting") 
            (get can-extend-voting-periods priv-data)
            (if (is-eq privilege-type "moderate-discussions") 
              (get can-moderate-discussions priv-data)
              false
            )
          )
        )))
      (ok false)
    )
  )
)

;; Read-only functions
(define-read-only (get-member-reputation (member principal))
  (map-get? member-reputation { member: member })
)

(define-read-only (get-member-badge (member principal) (badge-type (string-ascii 30)))
  (map-get? member-badges { member: member, badge-type: badge-type })
)

(define-read-only (get-reputation-tier-info (tier uint))
  (map-get? reputation-tiers { tier: tier })
)

(define-read-only (get-member-privileges (member principal))
  (map-get? member-privileges { member: member })
)

(define-read-only (get-merit-action (member principal) (action-id uint))
  (map-get? merit-actions { member: member, action-id: action-id })
)

(define-read-only (get-participation-history (member principal) (period uint))
  (map-get? participation-history { member: member, period: period })
)

(define-read-only (get-reputation-parameters)
  {
    base-accuracy-weight: (var-get base-voting-accuracy-weight),
    merit-per-vote: (var-get merit-points-per-vote),
    merit-per-proposal: (var-get merit-points-per-proposal),
    calculation-period: (var-get reputation-calculation-period)
  }
)

;; Get member governance summary
(define-read-only (get-member-governance-summary (member principal))
  (let
    (
      (base-member (contract-call? .VCC get-member member))
      (reputation-data (map-get? member-reputation { member: member }))
      (privileges-data (map-get? member-privileges { member: member }))
    )
    (ok {
      base-info: base-member,
      reputation: reputation-data,
      privileges: privileges-data,
      effective-voting-weight: (match (get-effective-voting-weight member)
        success success
        error u0
      )
    })
  )
)

;; Initialize the reputation system
(initialize-reputation-tiers)