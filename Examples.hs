
module Examples where

import ND

-- a couple simple problems that standard counterfactual reasoning can handle

shoot = diagram [dead] [True,True]
  where pull = "Pull" :<- Input
        jam  = "Jam"  :<- Input
        dead = "Dead" :<- Stim [pull] `Inhib` [jam]

doc = diagram [dead] [True,True]
  where doc  = "Doctor" :<- Input
        sick = "Sick"   :<- Input
        cure = "Cure"   :<- Stim [doc]
        dead = "Dead"   :<- Stim [sick] `Inhib` [cure]

-- now the trickier problems

-- desert traveler problem (preemption)
desert = diagram [dead] [True,True]
  where poison = "Poison" :<- Input
        poke   = "Poke"   :<- Input
        drink  = "Drink"  :<- Stim [poison] `Inhib` [poke]
        thirst = "Thirst" :<- Stim [poke]
        dead   = "Dead"   :<- Stim [drink,thirst]

-- two doctors problem (symmetric overdetermination by omission)
twoDocs = diagram [dead] [False,False]
  where docA = "A" :<- Input
        docB = "B" :<- Input
        cure = "Cure" :<- Thick 2 [docA,docB]
        sick = "Sick" :<- Const True
        dead = "Dead" :<- Stim [sick] `Inhib` [cure] 

boulder = diagram [dead] [True]
  where boulder = "Boulder" :<- Input
        duck    = "Duck"    :<- Stim [boulder] `IsKind` Act
        dead    = "Dead"    :<- Stim [boulder] `Inhib` [duck]

-- boulder problem (transitivity)
boulder' = diagram [live] [True]
  where boulder = "Boulder" :<- Input
        health  = "Health"  :<- Const True
        duck    = "Duck"    :<- Stim [boulder] `IsKind` Act
        crush   = "Crush"   :<- Stim [boulder] `Inhib`  [duck]
        live    = "Live"    :<- Stim [health]  `Inhib`  [crush]

-- assassin-guard problem
assassin = diagram [live] [True]
  where health   = "Health"   :<- Const True
        assassin = "Assassin" :<- Input
        guard    = "Guard"    :<- Stim [poison] `IsKind` Act
        poison   = "Poison"   :<- Stim [assassin]
        antidote = "Antidote" :<- Stim [guard]
        toxin    = "Toxin"    :<- XOR  [poison,antidote]
        live     = "Live"     :<- Stim [health] `Inhib` [toxin]

-- one liner graph test...
test = "Test" :<- Const True `Inhib` ["Thick" :<- Thick 2 ["A":<-Input,"B":<-Input]]
