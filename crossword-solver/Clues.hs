module Clues where
 
import Types

clue :: Int -> Clue
clue 1
  = ("companion shredded corset",6) -- ESCORT
clue 2
  = ("notice in flying coat", 6) -- JACKET 
clue 3
  = ("companion found in oklahoma terminal", 4) -- MATE
clue 4
  = ("a new member returned a woman", 6) -- ANGELA
clue 5
  = ("pause at these i fancy", 8) -- Everyman 3526, clue 1   ["athetise","hesitate"] 
clue 6
  = ("ankle was twisted in ballet", 8) -- Everyman 3526, clue 3
clue 7
  = ("flyer needed by funfair manager", 6) -- AIRMAN
clue 8
  = ("put food in this stuff on barge at sea", 9) -- TUCKER BAG
clue 9
  = ("notice supervisor is going nuts at first", 4) -- SIGN
clue 10
  = ("animal is mistake crossing one river", 7) -- GIRAFFE
clue 11
  = ("maria not a fickle lover", 9) -- INAMORATA
clue 12
  = ("hope for high praise", 6) -- ASPIRE
clue 13
  = ("Not fed partly twigged", 5) -- NOTED
clue 14 
  = ("Messy bit of lung next to part of kempton", 7) -- UNKEMPT
clue 15 
  = ("bee leaves branch for station", 5) -- RANCH
clue 16
  = ("Man changing line around Gateshead", 5) -- NIGEL
clue 17 
  = ("Animal returns to grass", 4) -- REED
clue 18 
  = ("bums mix for deals without energy", 9) -- FREELOADS
clue 19 -- PLUTOCRATS
  = ("liberal posh wearing platinum with fancy cars to give rich people", 10)
clue 20 -- SIGNAGE
  = ("indications show surprising gains for example after recovery",7)
clue 21 -- LEARNED
  = ("Scholarly head of languages brought in", 7)
clue 22 -- AZALEA [G26,499]
  = ("A zebra dropping guts, munching bitter plant",6)
clue 23 -- RETRACT
  = ("Withdraw Carter - about time!",7)
clue 24 -- STUN G26508
  = ("Heads turn in shock",4)
clue 25 -- OFFERS - not Ximenean (Guardian), but solvable 26508
  = ("Proposals for heartless transgressors",6) 
clue 26 -- ATTENDANT g26508
  = ("Servant, a sober worker, receives tip", 9)
--clue 27
--  = ("Its served on board found in apricot tarts", 7) -- ricotta
clue 27
  = ("one hundred and one late orders demanded first in fort",7) -- citadel
clue 28
  = ("cheese found in apricot tarts", 7) -- ricotta
clue 29
  = ("cutting ditch next to a new junction", 9) -- TRENCHANT
clue 30 
  = ("The reason a son left location", 5) -- THERE
--
-- Guardian clues 10/6/16
--
clue 31
  = ("info about large valley", 4) -- glen
clue 32
  = ("vegetable everyone put in picture", 7) -- shallot
clue 33 -- conglomerations
  = ("Left to invade African country with armies, not revolutionary masses",15)
clue 34
  = ("bird crowns at odds with backward staff", 6) -- condor
clue 35
  = ("part of boss-head, even money flipper", 4) -- shoe
clue 36 -- EXPENSIVE with indirect take-outs turned on
  = ("opening can of nails treck mostly back to event", 7) -- concert
clue 37 -- G27852 BAD FORM: subtraction from anagram -> STORM. Unsolvable.
  = ("Nostromo wrecked on ocean initially lost in typhoon", 5)
clue 38 -- G27859 incorrect
  = ("Improper behaviour finally stops in centre court", 9)
-- DISBURSEMENT
-- The anagram needs a subtraction of a synonym of partner (ALLY)
-- as an argument. A really nasty indirect anagram.
clue 39
  = ("Partner having left, really in debt, sums wasted, money spent", 12)
-- Solvable but no synonym for answer
clue 40
  = ("Poorly made Russian fighter returns with a bang",8) -- gimcrack
clue 41
  = ("Skin disease going around English riding school",6) -- manege
clue 42
  = ("Limit area where cattle may graze",5) -- range
clue 43
  = ("They take advantage of a sailor needing employers",7) -- abusers
clue 44
  = ("Package returned has address with added detail",9) -- elaborate
clue 45 -- California. Distribute peeling G27247
  = ("Peeling paint, profit slack, upset, in a state", 10) 
clue 46 -- DECOMPOSE
  = ("Ask to support alternative comedy, mostly rot", 9)
clue 47 -- G27269 reconstructed
  = ("After disturbance, centre court's crowd finally gets composed again", 13)
clue 48 -- Spec 2402 aphonia 
  = ("Extract from Telegraph on iatrogenic voice loss", 7)
clue 49 -- mauls Spec 2402
  = ("University dons strike back in attacks",5)
clue 50 -- aperiodic Spec 2402
  = ("Irregular cricket match in Morecambe covered by news agency", 9)
-- Nice example of subtraction under anagram
clue 51 -- outbursts Spec 2401
  = ("Fulminations from Brutus lost without Latin in translation", 9)
clue 52 -- vanuatu Spec 2401 VERY COMPLEX
  = ("Learner leaves Tuvalu, out touring a new island group", 7)
clue 53 -- tooart Spec 2390
  = ("Tree rings covered in acid", 6)
clue 54 -- hide G28104
  = ("Skin somewhat ticklish, I declare", 4)
clue 55 -- incur I8962
  = ("Suffer first signs of illness near central Ugandan region", 5)
clue 56 -- tosspots E3832
  = ("Old drunkards hurl crockery", 8)
clue 57 -- unsophisticated g28072
  = ("Simple Harry Houdini act with steps", 15)
clue 58 -- helvetica E3811
  = ("Evil cheat desecrated font", 9)
clue 59 -- eros G28077
  = ("God is angry on being overthrown", 4)
clue 60 -- chancey G28102
  = ("Saint even broke into two churches? Yes, that's risky", 7)
clue 61 -- castle E3501
  = ("Stronghold, one sacked in former kingdom", 6)
clue 62 -- ducked I1572
  = ("Avoided broadcast channel", 6)
-- Good one for eval cacheing: solveAll - 94s -> 69s.
clue 63 -- ileum G28060
  = ("Brought up breakfast cereal, bypassing the beginning of stomach and small intestine", 5)
clue 64 -- unkempt G28104
  = ("Messy bed vacated, case dismissed", 7)
clue 65 -- euchred S2407
-- Many readings, inc. end of <match had leftist> which also
-- solves. Q: How to spot the most obvious?
  = ("After hint turned up, end of match had leftist outwitted", 7)
clue 66 -- macaroon S2472
  = ("Coat and small jumper covered by an almondy snack", 8)
clue 67 -- noctua S2472
  = ("After new month turned up gold moth", 6)
clue 68 -- forelimb S2472
  = ("Member warning politician pocketing millions", 8)
clue 69 -- bacon G28232
  = ("Forbid eating firm meat", 5)
clue 70 -- cloudless G28232
  = ("Fine clues sold for a pound", 9)
clue 71 -- athletics S2474
  = ("Sport in which holder not even in the highest places",9)
clue 72 -- eisells S2474
  = ("In denims regularly market vinegars", 7)

paperClue 1
  = ("Do run to church", 6) -- fleece
paperClue 2
  = ("Body essence", 6) -- entity
paperClue 3 -- dissolute
  = ("Loose reconstruction of old suites", 9)
paperClue 4
  = ("Cancel article in disgust", 6) -- repeal
paperClue 5 
  = ("Rule in theatre ignored", 5) -- reign
paperClue 6
  = ("pawn in duel regularly set aside",5) -- annul E3694
paperClue 7
  = ("furious buccaneer deprived of power",5) -- irate E3694
paperClue 8
  = ("flightless bird caught finally in ditch",4) -- moat E3693
paperClue 9 -- fined E3694)
  = ("reported discovery, penalised",5) 
paperClue 10 -- E3672 entail
  = ("call for extremes of exertion before end", 6)
paperClue 11 -- G27269 stretta
  = ("musical passage starts to sound terrific really exciting to the audience",7)
paperClue 12 -- G27285 parisian
  = ("French citizen taking part in sin - and endlessly",8)
paperClue 13 -- G27288 stir
  = ("commotion caused by heads of state talking in riddles", 4)
paperClue 14 -- G27288 intricate
  = ("Fancy popular gallery keeping racist leader in charge!",9)
paperClue 15 -- G27288 dinner
  = ("revolutionary coming back to eat pub grub?",6)
paperClue 16 -- G26494 shrieked
  = ("\"An ogre is squashing me!\", the little prince yelled", 8) 
paperClue 17 -- apposite
  = ("A quiet Italian river location may be suitable",8)
paperClue 18 -- self-important
  = ("Harry Potter film initially not as pompous", 13)
paperClue 19 -- players
  = ("team members chewing parsley", 7)
paperClue 20 -- dedicator
  = ("princess takes little prince in car to see consignor",9)
paperClue 21 -- cello
  = ("Instrument made from two circles", 5)
paperClue 22 -- G24703 thai
  = ("Bond's said to be Asian", 4)
paperClue 23 -- G27269 oates
  = ("Conspirator established a ring for revolution", 5)
paperClue 24 -- G27269 elgin
  = ("English spirit? Little left in a Scottish place", 5)
paperClue 25 -- G24700 dense
  = ("Dim woman lacks ego", 5) 
paperClue 26 -- G24700 wrong
  = ("Regularly swore to love no-good criminal", 5)
paperClue 27 -- G24700 derange
  = ("Madden monarch who's in endless peril", 7)
paperClue 28 -- G24700 gelignite
  = ("Element added to brown coal to make explosive", 9)
paperClue 29 -- G24700 nicotiana
  = ("CIA involvement with nation that produces tobacco", 9)
paperClue 30 -- G24700 rotterdam
  = ("city cad supported by mother", 9)
paperClue 31 -- G24700 ramadam
  = ("Fast-moving drama starts to attract notice", 7)
paperClue 32 -- G24700 krone
  = ("Currency used over in Yemen or Kuwait", 5)
paperClue 33 -- proposal (Azed slip No 2,378)
  = ("rough loo paper's leaving one's rear tender", 8)
paperClue 34 -- proposal (Azed slip No 2,378)
  = ("Bid for bone china cups", 8)
paperClue 35 -- G22793 pilchards
  = ("Fish and chips cooked with lard", 9)
paperClue 36 -- G28515 empty
  = ("Vain politician yet to be ruffled outside",5)
paperClue 37 -- G28104 hide
  = ("Skin somewhat ticklish, I declare", 4)
paperClue 38 -- E2975 lager
  = ("Magnificent drink knocked back", 5)
paperClue 39 -- Telegraph charade
  = ("cleaner with little energy's given notice that needs to be worked out",7)

-- https://www.crosswordgiant.com/crossword-puzzle/121080/The-Times-Cryptic/Times-Cryptic-26783-July-21-2017/2017-07-21
-- grass: can transform partof (reverse text)) to reverse (partof text)...
times 1
  = ("Shop less, arguably, after going around stores", 5) 
times 2
  = ("Arab I transported across Bible Land", 7)
times 3
  = ("hurry on by to see holiday apartment",9)
times 4
  = ("A poem regularly reflecting on mostly sad state", 5)
-- Solvable with reverse dict.
times 5
  = ("A poem regularly reflecting on mostly sad state of islands", 5)
times 6
  = ("title or medal for wrestling", 7)
times 7 -- pooh-poohs
  = ("Upset, small twin rings and knocks",9)
times 8 -- afraid
  = ("A hand round brother, cowering", 6)
times 9 -- wield 26783
  = ("One involved in joint exercise", 5)
times 10 -- fibres
  = ("Threads of story remain incomplete", 6)
times 11
-- SHAM [forged] + EL ESS [two letters "written out"]
-- shameless.
  = ("Forged letters written out in bold", 9)
-- threw
times 12
  = ("Called out thanks to cast", 5)
-- was
times 13
  = ("Used to be clean after wiping hard", 3)
-- All the following from Times 27,550...
-- Richter: cannot be solved without indirect subtext
times 14
  = ("Fat Hilary maybe briefly rocking scale", 7)
times 15 -- lineage
  = ("Following on, batting to open the day", 7)
times 16 -- bushbaby
  = ("A maiden leaves surprise new arrival, a small primate", 8)
times 17 -- absent
  = ("A criminal admits beginning to sell out", 6)
times 18 -- reeve
  = ("Old magistrate constantly returning to drink last of wine", 5)
times 19 -- chasten
  = ("cow caught fly",7)
times 20 -- hiked
  = ("Marched president into empty helipad", 5)
times 21 -- espresso
  = ("Energy beginning to soar, keep going without finishing coffee", 8)
times 22 -- edelweiss
  = ("Flower I planted in weedless ground", 9)
times 23 -- sheepskin
  = ("Warm garment initially sheltering uriah's family?", 9)
times 24 -- squashed
  = ("Son rejected flat", 8)
times 25 -- amiss
  = ("Wrong a lady", 5)


guardian 1 -- tearful (27396)
  = ("distressed tenor given a wigging",7)
guardian 2 -- marinate (27396)
  = ("steep terrace unoccupied behind mooring area",8)
guardian 3 -- extra (27396)
  = ("more cunning times puzzle ultimately rejected",5)
guardian 4 -- chairman (28023)
  = ("In revolutionary china, run by Mao, mostly as leader", 8)

-- Position 1396 from 1764 parses
-- There are MANY solutions, "le" comes from "aLE" in the above
expensiveClue 1 -- G27269 chronicle
  = ("story of church fellow in charge, having drink, losing head",9) 
expensiveClue 2 -- G27859 licence. SUB () from ANAGRAM ...
-- lack of restraint isn't a synonym of licence, but still...
  = ("Liberal in church welcoming church's lack of restraint", 7)
expensiveClue 3 -- expedient (G27269)
  = ("No longer taking exercise, one in depression needed to get fit", 9)


-- Solvable using reverse dictionary...
--
-- Obscenity. 
clue' 1
  = ("Naughty word shocked nicest boy", 9) 
-- mayoress
clue' 2
  = ("Official service covers times gone by", 8)
clue' 3 -- dialectic G27269
  = ("System of reasoning set up has frantic hospital missing out", 9)
clue' 4
  = ("novel by incomer in its early stages", 9)

-- Castrate. Takes testicles would work with rev dic
clue'' 2
  = ("Actors' fees take balls", 8)
-- Decompose. Needs anagram after mostly is applied. Easily fixed by allowing
-- literal subtext under an anagram (needs extra parsing rule for subtext
-- without synonyms.

userClue 1 
  = ("One hundred and fifty eight head back to country", 7)

-- Subtraction tests

sub 1
  = ("cowl without century bird",3) -- owl
sub 2
  = ("decided without police act",4) -- deed/over
sub 3 
  = ("Fuming having lost silver wreath", 4) -- ring (fuming->curing/raging)
sub 4
  = ("depict bad accident without police",5) -- enact
sub 5
  = ("Badly ported losing short time for junkie", 5) -- doper

-- Indirect take-outs...
-- First
indF 1
  = ("First meandering breeze", 4) -- wind (winding)
indF 2
  = ("Agree to fool recipe, initially", 7) -- conform etc. (con formula)
indF 3
  = ("First bash wears acquittal", 6) -- pardon (party dons)

-- Last
indL 1
  = ("Haggling ends fishing", 7) -- angling
indL 2
  = ("Flop requesting ends sunbathing", 7) -- basking (bomb asking)
indL 3
  = ("Throw party, ending recess", 5) -- stash (first bash/thrash...)

-- Middle
indM 1
  = ("Friend in gem's centre", 3) -- date (mandatee)
indM 2
  = ("Encircle swamp - bad in the middle", 5) -- orbit (moor bitter)
indM 3
  = ("Lease glum workers inside", 4) -- rent (grey ants)

-- End
indE 1
  = ("Wine from instrument ends", 6) -- claret (clarinet)
indE 2
  = ("Choose worker's extreme measures", 5) -- pints (pick ants)
indE 3
  = ("Prank from concern - terrific extremes", 5) -- caper (care super)
indE 4
  = ("Twin egg-shaped extremes of instrument", 4) -- tool (two oval)

-- Indirect reversal take-outs
indRev 1
  = ("Twin egg-shaped backs extremes of bounty", 4) -- loot (above)
indRev 2
  = ("Shop, except spat after going around stores", 5) -- grass

--
-- The ultimate...
-- Generates a whole bunch of solutions
--
indAnagram 1
  = ("Bird mess central to mealy workers", 4)
