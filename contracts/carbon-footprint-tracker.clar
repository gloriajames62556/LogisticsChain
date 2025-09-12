;; Carbon Footprint Tracker - Environmental Impact Assessment for Supply Chain
;; Enables companies to track, measure, and offset carbon emissions from logistics operations

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u400))
(define-constant err-not-found (err u401))
(define-constant err-invalid-input (err u402))
(define-constant err-insufficient-funds (err u403))
(define-constant err-already-exists (err u404))
(define-constant err-offset-unavailable (err u405))
(define-constant err-goal-not-met (err u406))

;; Data variables for emission factors and pricing
(define-data-var co2-per-km-truck uint u250) ;; grams CO2 per km for truck
(define-data-var co2-per-km-ship uint u30)  ;; grams CO2 per km for ship
(define-data-var co2-per-km-plane uint u500) ;; grams CO2 per km for plane
(define-data-var carbon-credit-price uint u50) ;; STX per ton CO2 offset
(define-data-var next-offset-id uint u1)

;; Transport mode emission factors (grams CO2 per km per kg of cargo)
(define-map transport-emissions
    { transport-mode: (string-ascii 20) }
    { 
        co2-per-km-per-kg: uint,
        efficiency-rating: uint,
        renewable-energy-factor: uint
    }
)

;; Shipment carbon footprint tracking
(define-map shipment-emissions
    { shipment-id: uint }
    {
        total-co2-grams: uint,
        distance-km: uint,
        transport-mode: (string-ascii 20),
        cargo-weight-kg: uint,
        fuel-efficiency-score: uint,
        calculated-at: uint
    }
)

;; Company environmental goals and tracking
(define-map company-sustainability-goals
    { company-id: uint }
    {
        annual-co2-limit-kg: uint,
        current-co2-emissions-kg: uint,
        carbon-neutral-target-date: uint,
        green-transport-percentage: uint,
        offset-purchased-kg: uint,
        created-at: uint
    }
)

;; Carbon offset marketplace
(define-map carbon-offsets
    { offset-id: uint }
    {
        provider: principal,
        co2-amount-kg: uint,
        price-per-kg: uint,
        verification-standard: (string-ascii 50),
        project-type: (string-ascii 50),
        available: bool,
        created-at: uint
    }
)

;; Company offset purchases
(define-map company-offset-purchases
    { company-id: uint, offset-id: uint }
    {
        amount-purchased-kg: uint,
        total-paid: uint,
        purchased-at: uint,
        shipment-ids: (list 10 uint)
    }
)

;; Green route alternatives
(define-map eco-friendly-routes
    { origin-id: uint, destination-id: uint }
    {
        standard-co2-kg: uint,
        eco-route-co2-kg: uint,
        co2-savings-kg: uint,
        additional-cost: uint,
        transport-mode: (string-ascii 20),
        last-updated: uint
    }
)

;; Initialize transport emission factors
(define-public (initialize-emission-factors)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (try! (set-transport-emission "truck" u200 u70 u10))
        (try! (set-transport-emission "ship" u25 u95 u40))
        (try! (set-transport-emission "plane" u400 u40 u20))
        (try! (set-transport-emission "rail" u50 u90 u60))
        (try! (set-transport-emission "electric-truck" u100 u85 u90))
        (ok true)
    )
)

;; Set or update transport emission factors (admin only)
(define-public (set-transport-emission 
    (transport-mode (string-ascii 20)) 
    (co2-per-km-per-kg uint) 
    (efficiency-rating uint)
    (renewable-energy-factor uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (<= efficiency-rating u100) err-invalid-input)
        (asserts! (<= renewable-energy-factor u100) err-invalid-input)
        (map-set transport-emissions
            { transport-mode: transport-mode }
            {
                co2-per-km-per-kg: co2-per-km-per-kg,
                efficiency-rating: efficiency-rating,
                renewable-energy-factor: renewable-energy-factor
            }
        )
        (ok true)
    )
)

;; Calculate and record shipment carbon footprint
(define-public (calculate-shipment-emissions 
    (shipment-id uint)
    (distance-km uint)
    (transport-mode (string-ascii 20))
    (cargo-weight-kg uint))
    (let
        (
            (company-data (unwrap! (contract-call? .LogisticsChain get-company-by-principal tx-sender) err-not-found))
            (emission-factor (unwrap! (map-get? transport-emissions { transport-mode: transport-mode }) err-not-found))
            (base-co2-grams (* (* distance-km cargo-weight-kg) (get co2-per-km-per-kg emission-factor)))
            (efficiency-adjustment (/ (* base-co2-grams (get efficiency-rating emission-factor)) u100))
            (renewable-adjustment (/ (* efficiency-adjustment (- u100 (get renewable-energy-factor emission-factor))) u100))
            (total-co2-grams renewable-adjustment)
            (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        
        ;; Verify user has active subscription through LogisticsChain
        (asserts! (contract-call? .LogisticsChain is-subscription-active (get company-id company-data)) err-not-authorized)
        
        ;; Record emission calculation
        (map-set shipment-emissions
            { shipment-id: shipment-id }
            {
                total-co2-grams: total-co2-grams,
                distance-km: distance-km,
                transport-mode: transport-mode,
                cargo-weight-kg: cargo-weight-kg,
                fuel-efficiency-score: (get efficiency-rating emission-factor),
                calculated-at: current-time
            }
        )
        
        ;; Update company's total emissions
        (update-company-emissions (get company-id company-data) (/ total-co2-grams u1000))
        
        (ok total-co2-grams)
    )
)

;; Set sustainability goals for a company
(define-public (set-sustainability-goals 
    (annual-co2-limit-kg uint)
    (carbon-neutral-target-date uint)
    (green-transport-percentage uint))
    (let
        (
            (company-data (unwrap! (contract-call? .LogisticsChain get-company-by-principal tx-sender) err-not-found))
            (company-id (get company-id company-data))
            (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        
        (asserts! (> annual-co2-limit-kg u0) err-invalid-input)
        (asserts! (> carbon-neutral-target-date current-time) err-invalid-input)
        (asserts! (<= green-transport-percentage u100) err-invalid-input)
        
        (map-set company-sustainability-goals
            { company-id: company-id }
            {
                annual-co2-limit-kg: annual-co2-limit-kg,
                current-co2-emissions-kg: u0,
                carbon-neutral-target-date: carbon-neutral-target-date,
                green-transport-percentage: green-transport-percentage,
                offset-purchased-kg: u0,
                created-at: current-time
            }
        )
        
        (ok true)
    )
)

;; Create carbon offset offering
(define-public (create-carbon-offset 
    (co2-amount-kg uint)
    (price-per-kg uint)
    (verification-standard (string-ascii 50))
    (project-type (string-ascii 50)))
    (let
        (
            (offset-id (var-get next-offset-id))
            (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        
        (asserts! (> co2-amount-kg u0) err-invalid-input)
        (asserts! (> price-per-kg u0) err-invalid-input)
        
        (var-set next-offset-id (+ offset-id u1))
        
        (map-set carbon-offsets
            { offset-id: offset-id }
            {
                provider: tx-sender,
                co2-amount-kg: co2-amount-kg,
                price-per-kg: price-per-kg,
                verification-standard: verification-standard,
                project-type: project-type,
                available: true,
                created-at: current-time
            }
        )
        
        (ok offset-id)
    )
)

;; Purchase carbon offsets
(define-public (purchase-carbon-offset 
    (offset-id uint)
    (amount-kg uint)
    (shipment-ids (list 10 uint)))
    (let
        (
            (company-data (unwrap! (contract-call? .LogisticsChain get-company-by-principal tx-sender) err-not-found))
            (company-id (get company-id company-data))
            (offset (unwrap! (map-get? carbon-offsets { offset-id: offset-id }) err-not-found))
            (total-cost (* amount-kg (get price-per-kg offset)))
            (current-time (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        
        (asserts! (get available offset) err-offset-unavailable)
        (asserts! (<= amount-kg (get co2-amount-kg offset)) err-invalid-input)
        (asserts! (> amount-kg u0) err-invalid-input)
        
        ;; Transfer payment to offset provider
        (unwrap! (stx-transfer? total-cost tx-sender (get provider offset)) err-insufficient-funds)
        
        ;; Update offset availability
        (map-set carbon-offsets
            { offset-id: offset-id }
            (merge offset {
                co2-amount-kg: (- (get co2-amount-kg offset) amount-kg),
                available: (> (- (get co2-amount-kg offset) amount-kg) u0)
            })
        )
        
        ;; Record purchase
        (map-set company-offset-purchases
            { company-id: company-id, offset-id: offset-id }
            {
                amount-purchased-kg: amount-kg,
                total-paid: total-cost,
                purchased-at: current-time,
                shipment-ids: shipment-ids
            }
        )
        
        ;; Update company sustainability goals
        (update-company-offsets company-id amount-kg)
        
        (ok true)
    )
)

;; Compare eco-friendly vs standard routing options
(define-public (calculate-green-route-savings 
    (origin-id uint)
    (destination-id uint)
    (cargo-weight-kg uint)
    (standard-distance-km uint)
    (eco-distance-km uint)
    (eco-transport-mode (string-ascii 20)))
    (let
        (
            (standard-emission (unwrap! (map-get? transport-emissions { transport-mode: "truck" }) err-not-found))
            (eco-emission (unwrap! (map-get? transport-emissions { transport-mode: eco-transport-mode }) err-not-found))
            (standard-co2-kg (/ (* (* standard-distance-km cargo-weight-kg) (get co2-per-km-per-kg standard-emission)) u1000))
            (eco-co2-kg (/ (* (* eco-distance-km cargo-weight-kg) (get co2-per-km-per-kg eco-emission)) u1000))
            (co2-savings-kg (if (> standard-co2-kg eco-co2-kg) (- standard-co2-kg eco-co2-kg) u0))
            (additional-cost (* co2-savings-kg u10)) ;; Estimated additional cost for eco-friendly option
        )
        
        (map-set eco-friendly-routes
            { origin-id: origin-id, destination-id: destination-id }
            {
                standard-co2-kg: standard-co2-kg,
                eco-route-co2-kg: eco-co2-kg,
                co2-savings-kg: co2-savings-kg,
                additional-cost: additional-cost,
                transport-mode: eco-transport-mode,
                last-updated: (default-to u0 (get-stacks-block-info? time (- stacks-block-height u1)))
            }
        )
        
        (ok co2-savings-kg)
    )
)

;; Private helper functions
(define-private (update-company-emissions (company-id uint) (additional-co2-kg uint))
    (let
        (
            (current-goals (map-get? company-sustainability-goals { company-id: company-id }))
        )
        
        (match current-goals
            goals (map-set company-sustainability-goals
                { company-id: company-id }
                (merge goals {
                    current-co2-emissions-kg: (+ (get current-co2-emissions-kg goals) additional-co2-kg)
                })
            )
            true
        )
    )
)

(define-private (update-company-offsets (company-id uint) (offset-amount-kg uint))
    (let
        (
            (current-goals (map-get? company-sustainability-goals { company-id: company-id }))
        )
        
        (match current-goals
            goals (map-set company-sustainability-goals
                { company-id: company-id }
                (merge goals {
                    offset-purchased-kg: (+ (get offset-purchased-kg goals) offset-amount-kg)
                })
            )
            true
        )
    )
)

;; Read-only functions
(define-read-only (get-shipment-emissions (shipment-id uint))
    (map-get? shipment-emissions { shipment-id: shipment-id })
)

(define-read-only (get-company-sustainability (company-id uint))
    (map-get? company-sustainability-goals { company-id: company-id })
)

(define-read-only (get-carbon-offset (offset-id uint))
    (map-get? carbon-offsets { offset-id: offset-id })
)

(define-read-only (get-transport-emission-factor (transport-mode (string-ascii 20)))
    (map-get? transport-emissions { transport-mode: transport-mode })
)

(define-read-only (get-eco-route-comparison (origin-id uint) (destination-id uint))
    (map-get? eco-friendly-routes { origin-id: origin-id, destination-id: destination-id })
)

(define-read-only (calculate-carbon-neutrality-status (company-id uint))
    (match (map-get? company-sustainability-goals { company-id: company-id })
        goals (let
            (
                (net-emissions (if (> (get current-co2-emissions-kg goals) (get offset-purchased-kg goals))
                                  (- (get current-co2-emissions-kg goals) (get offset-purchased-kg goals))
                                  u0))
                (carbon-neutral (is-eq net-emissions u0))
                (progress-percentage (if (> (get current-co2-emissions-kg goals) u0)
                                       (/ (* (get offset-purchased-kg goals) u100) (get current-co2-emissions-kg goals))
                                       u100))
            )
            (ok {
                carbon-neutral: carbon-neutral,
                net-emissions-kg: net-emissions,
                progress-percentage: progress-percentage,
                offset-deficit-kg: net-emissions
            })
        )
        err-not-found
    )
)

(define-read-only (get-emission-reduction-recommendations (company-id uint))
    (match (map-get? company-sustainability-goals { company-id: company-id })
        goals (let
            (
                (emissions-over-limit (if (> (get current-co2-emissions-kg goals) (get annual-co2-limit-kg goals))
                                        (- (get current-co2-emissions-kg goals) (get annual-co2-limit-kg goals))
                                        u0))
                (recommended-offsets (+ emissions-over-limit u1000)) ;; Extra buffer for carbon negative impact
            )
            (ok {
                over-limit: (> (get current-co2-emissions-kg goals) (get annual-co2-limit-kg goals)),
                excess-emissions-kg: emissions-over-limit,
                recommended-offset-purchase-kg: recommended-offsets,
                estimated-offset-cost: (* recommended-offsets (var-get carbon-credit-price))
            })
        )
        err-not-found
    )
)
