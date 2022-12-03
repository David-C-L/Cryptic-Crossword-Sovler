import os

from flask import Flask, request
from flask_cors import CORS, cross_origin

from morseInterface import solveClue, solveBuiltinClue

app = Flask(__name__)
cors = CORS(app)

test_data = [{'clue': 'companion shredded corset', 'length': 6, 'answers': ['ESCORT']},
             {'clue': 'notice in flying coat', 'length': 6, 'answers': ['JACKET']},
             {'clue': 'companion found in oklahoma terminal', 'length': 4, 'answers': ['MATE']},
             {'clue': 'a new member returned a woman', 'length': 6, 'answers': ['ANGELA']},
             {'clue': 'flyer needed by funfair manager', 'length': 6, 'answers': ['AIRMAN']},
             {'clue': 'put food in this stuff on barge at sea', 'length': 9, 'answers': ['TUCKER BAG']},
             {'clue': 'notice supervisor is going nuts at first', 'length': 4, 'answers': ['SIGN']},
             {'clue': 'animal is mistake crossing one river', 'length': 7, 'answers': ['GIRAFFE']},
             {'clue': 'maria not a fickle lover', 'length': 9, 'answers': ['INAMORATA']},
             {'clue': 'hope for high praise', 'length': 6, 'answers': ['ASPIRE']},
             {'clue': 'not fed partly twigged', 'length': 5, 'answers': ['NOTED']},
             {'clue': 'messy bit of lung next to part of kempton', 'length': 7, 'answers': ['UNKEMPT']},
             {'clue': 'bee leaves branch for station', 'length': 5, 'answers': ['RANCH']},
             {'clue': 'man changing line around gateshead', 'length': 5, 'answers': ['NIGEL']},
             {'clue': 'animal returns to grass', 'length': 4, 'answers': ['REED']},
             {'clue': 'bums mix for deals without energy', 'length': 9, 'answers': ['FREELOADS']},
             {'clue': 'liberal posh wearing platinum with fancy cars to give rich people', 'length': 10,
              'answers': ['PLUTOCRATS']},
             {'clue': 'indications show surprising gains for example after recovery', 'length': 7,
              'answers': ['SIGNAGE']},
             {'clue': 'scholarly head of languages brought in', 'length': 7, 'answers': ['LEARNED']},
             {'clue': 'a zebra dropping guts, munching bitter plant', 'length': 6, 'answers': ['AZALEA']},
             {'clue': 'withdraw carter - about time!', 'length': 7, 'answers': ['RETRACT']},
             {'clue': 'heads turn in shock', 'length': 4, 'answers': ['STUN']},
             {'clue': 'proposals for heartless transgressors', 'length': 6, 'answers': ['OFFERS']},
             {'clue': 'servant, a sober worker, receives tip', 'length': 9, 'answers': ['ATTENDANT']},
             {'clue': 'one hundred and one late orders demanded first in fort', 'length': 7, 'answers': ['CITADEL']},
             {'clue': 'cheese found in apricot tarts', 'length': 7, 'answers': ['RICOTTA']},
             {'clue': 'cutting ditch next to a new junction', 'length': 9, 'answers': ['TRENCHANT']},
             {'clue': 'the reason a son left location', 'length': 5, 'answers': ['THERE']},
             {'clue': 'info about large valley', 'length': 4, 'answers': ['GLEN']},
             {'clue': 'vegetable everyone put in picture', 'length': 7, 'answers': ['SHALLOT']},
             {'clue': 'left to invade african country with armies, not revolutionary masses', 'length': 15,
              'answers': ['CONGLOMERATIONS']},
             {'clue': 'bird crowns at odds with backward staff', 'length': 6, 'answers': ['CONDOR']},
             {'clue': 'part of boss-head, even money flipper', 'length': 4, 'answers': ['SHOE']},
             {'clue': 'opening can of nails treck mostly back to event', 'length': 7, 'answers': ['CONCERT']},
             {'clue': 'nostromo wrecked on ocean initially lost in typhoon', 'length': 5, 'answers': ['STORM']},
             {'clue': 'improper behaviour finally stops in centre court', 'length': 9, 'answers': ['INCORRECT']},
             {'clue': 'partner having left, really in debt, sums wasted, money spent', 'length': 12,
              'answers': ['DISBURSEMENT']},
             {'clue': 'poorly made russian fighter returns with a bang', 'length': 8, 'answers': ['GIMCRACK']},
             {'clue': 'skin disease going around english riding school', 'length': 6, 'answers': ['MANEGE']},
             {'clue': 'limit area where cattle may graze', 'length': 5, 'answers': ['RANGE']},
             {'clue': 'they take advantage of a sailor needing employers', 'length': 7, 'answers': ['ABUSERS']},
             {'clue': 'package returned has address with added detail', 'length': 9, 'answers': ['ELABORATE']},
             {'clue': 'peeling paint, profit slack, upset, in a state', 'length': 10, 'answers': ['CALIFORNIA']},
             {'clue': 'ask to support alternative comedy, mostly rot', 'length': 9, 'answers': ['DECOMPOSE']},
             {'clue': 'after disturbance, centre court\'s crowd finally gets composed again', 'length': 13,
              'answers': ['RECONSTRUCTED']},
             {'clue': 'extract from telegraph on iatrogenic voice loss', 'length': 7, 'answers': ['APHONIA']},
             {'clue': 'university dons strike back in attacks', 'length': 5, 'answers': ['MAULS']},
             {'clue': 'irregular cricket match in morecambe covered by news agency', 'length': 9,
              'answers': ['APERIODIC']},
             {'clue': 'fulminations from brutus lost without latin in translation', 'length': 9,
              'answers': ['OUTBURSTS']},
             {'clue': 'learner leaves tuvalu, out touring a new island group', 'length': 7, 'answers': ['VANUATU']},
             {'clue': 'tree rings covered in acid', 'length': 6, 'answers': ['TOOART']},
             {'clue': 'skin somewhat ticklish, i declare', 'length': 4, 'answers': ['HIDE']},
             {'clue': 'suffer first signs of illness near central ugandan region', 'length': 5, 'answers': ['INCUR']},
             {'clue': 'old drunkards hurl crockery', 'length': 8, 'answers': ['TOSSPOTS']},
             {'clue': 'simple harry houdini act with steps', 'length': 15, 'answers': ['UNSOPHISTICATED']},
             {'clue': 'evil cheat desecrated font', 'length': 9, 'answers': ['HELVETICA']},
             {'clue': 'god is angry on being overthrown', 'length': 4, 'answers': ['EROS']},
             {'clue': 'saint even broke into two churches? yes, that\'s risky', 'length': 7, 'answers': ['CHANCEY']},
             {'clue': 'stronghold, one sacked in former kingdom', 'length': 6, 'answers': ['CASTLE']},
             {'clue': 'avoided broadcast channel', 'length': 6, 'answers': ['DUCKED']},
             {'clue': 'brought up breakfast cereal, bypassing the beginning of stomach and small intestine',
              'length': 5, 'answers': ['ILEUM']},
             {'clue': 'messy bed vacated, case dismissed', 'length': 7, 'answers': ['UNKEMPT']},
             {'clue': 'after hint turned up, end of match had leftist outwitted', 'length': 7, 'answers': ['EUCHRED']},
             {'clue': 'coat and small jumper covered by an almondy snack', 'length': 8, 'answers': ['MACAROON']},
             {'clue': 'after new month turned up gold moth', 'length': 6, 'answers': ['NOCTUA']},
             {'clue': 'member warning politician pocketing millions', 'length': 8, 'answers': ['FORELIMB']},
             {'clue': 'forbid eating firm meat', 'length': 5, 'answers': ['BACON']},
             {'clue': 'fine clues sold for a pound', 'length': 9, 'answers': ['CLOUDLESS']},
             {'clue': 'sport in which holder not even in the highest places', 'length': 9, 'answers': ['ATHLETICS']},
             {'clue': 'in denims regularly market vinegars', 'length': 7, 'answers': ['EISELLS']},
             {'clue': 'Do run to church', 'length': 6, 'answers': ['FLEECE']},
             {'clue': 'Body essence', 'length': 6, 'answers': ['ENTITY']},
             {'clue': 'Loose reconstruction of old suites', 'length': 9, 'answers': ['DISSOLUTE']},
             {'clue': 'Cancel article in disgust', 'length': 6, 'answers': ['REPEAL']},
             {'clue': 'Rule in theatre ignored', 'length': 5, 'answers': ['REIGN']},
             {'clue': 'pawn in duel regularly set aside', 'length': 5, 'answers': ['ANNUL']},
             {'clue': 'furious buccaneer deprived of power', 'length': 5, 'answers': ['IRATE']},
             {'clue': 'flightless bird caught finally in ditch', 'length': 4, 'answers': ['MOAT']},
             {'clue': 'reported discovery, penalised', 'length': 5, 'answers': ['FINED']},
             {'clue': 'call for extremes of exertion before end', 'length': 6, 'answers': ['ENTAIL']},
             {'clue': 'musical passage starts to sound terrific really exciting to the audience', 'length': 7,
              'answers': ['STRETTA']},
             {'clue': 'French citizen taking part in sin - and endlessly', 'length': 8, 'answers': ['PARISIAN']},
             {'clue': 'commotion caused by heads of state talking in riddles', 'length': 4, 'answers': ['STIR']},
             {'clue': 'Fancy popular gallery keeping racist leader in charge!', 'length': 9, 'answers': ['INTRICATE']},
             {'clue': 'revolutionary coming back to eat pub grub?', 'length': 6, 'answers': ['DINNER']},
             {'clue': '\"An ogre is squashing me!\" the little prince yelled', 'length': 8, 'answers': ['SHRIEKED']},
             {'clue': 'A quiet Italian river location may be suitable', 'length': 8, 'answers': ['APPOSITE']},
             {'clue': 'Harry Potter film initially not as pompous', 'length': 13, 'answers': ['SELF-IMPORTANT']},
             {'clue': 'team members chewing parsley', 'length': 7, 'answers': ['PLAYERS']},
             {'clue': 'princess takes little prince in car to see consignor', 'length': 9, 'answers': ['DEDICATOR']},
             {'clue': 'Instrument made from two circles', 'length': 5, 'answers': ['CELLO']},
             {'clue': 'Bond\'s said to be Asian', 'length': 4, 'answers': ['THAI']},
             {'clue': 'Conspirator established a ring for revolution', 'length': 5, 'answers': ['OATES']},
             {'clue': 'English spirit? Little left in a Scottish place', 'length': 5, 'answers': ['ELGIN']},
             {'clue': 'Dim woman lacks ego', 'length': 5, 'answers': ['DENSE']},
             {'clue': 'Regularly swore to love no-good criminal', 'length': 5, 'answers': ['WRONG']},
             {'clue': 'Madden monarch who\'s in endless peril', 'length': 7, 'answers': ['DERANGE']},
             {'clue': 'Element added to brown coal to make explosive', 'length': 9, 'answers': ['GELIGNITE']},
             {'clue': 'CIA involvement with nation that produces tobacco', 'length': 9, 'answers': ['NICOTIANA']},
             {'clue': 'city cad supported by mother', 'length': 9, 'answers': ['ROTTERDAM']},
             {'clue': 'Fast-moving drama starts to attract notice', 'length': 7, 'answers': ['RAMADAM']},
             {'clue': 'Currency used over in Yemen or Kuwait', 'length': 5, 'answers': ['KRONE']},
             {'clue': 'rough loo paper\'s leaving one\'s rear tender', 'length': 8,
              'answers': ['PROPOSAL']},
             {'clue': 'Bid for bone china cups', 'length': 8, 'answers': ['PROPOSAL']},
             {'clue': 'Fish and chips cooked with lard', 'length': 9, 'answers': ['PILCHARDS']},
             {'clue': 'Vain politician yet to be ruffled outside', 'length': 5, 'answers': ['EMPTY']},
             {'clue': 'Skin somewhat ticklish, I declare', 'length': 4, 'answers': ['HIDE']},
             {'clue': 'Magnificent drink knocked back', 'length': 5, 'answers': ['LAGER']},
             {'clue': 'cleaner with little energy\'s given notice that needs to be worked out', 'length': 7,
              'answers': ['CHARADE']},
             {'clue': 'Upset, small twin rings and knocks', 'length': 9, 'answers': ['POOHPOOHS']},
             {'clue': 'A hand round brother, cowering', 'length': 6, 'answers': ['AFRAID']},
             {'clue': 'One involved in joint exercise', 'length': 5, 'answers': ['WIELD']},
             {'clue': 'Threads of story remain incomplete', 'length': 6, 'answers': ['FIBRES']},
             {'clue': 'Forged letters written out in bold', 'length': 9, 'answers': ['SHAMELESS']},
             {'clue': 'Called out thanks to cast', 'length': 5, 'answers': ['THREW']},
             {'clue': 'Used to be clean after wiping hard', 'length': 3, 'answers': ['WAS']},
             {'clue': 'Fat Hilary maybe briefly rocking scale', 'length': 7, 'answers': ['RICHTER']},
             {'clue': 'Following on, batting to open the day', 'length': 7, 'answers': ['LINEAGE']},
             {'clue': 'A maiden leaves surprise new arrival, a small primate', 'length': 8, 'answers': ['BUSHBABY']},
             {'clue': 'A criminal admits beginning to sell out', 'length': 6, 'answers': ['ABSENT']},
             {'clue': 'Old magistrate constantly returning to drink last of wine', 'length': 5, 'answers': ['REEVE']},
             {'clue': 'cow caught fly', 'length': 7, 'answers': ['CHASTEN']},
             {'clue': 'Marched president into empty helipad', 'length': 5, 'answers': ['HIKED']},
             {'clue': 'Energy beginning to soar, keep going without finishing coffee', 'length': 8,
              'answers': ['ESPRESSO']},
             {'clue': 'Flower I planted in weedless ground', 'length': 9, 'answers': ['EDELWEISS']},
             {'clue': 'Warm garment initially sheltering uriah\'s family?', 'length': 9, 'answers': ['SHEEPSKIN']},
             {'clue': 'Son rejected flat', 'length': 8, 'answers': ['SQUASHED']},
             {'clue': 'Wrong a lady', 'length': 5, 'answers': ['AMISS']},
             {'clue': 'distressed tenor given a wigging', 'length': 7, 'answers': ['TEARFUL']},
             {'clue': 'steep terrace unoccupied behind mooring area', 'length': 8, 'answers': ['MARINATE']},
             {'clue': 'more cunning times puzzle ultimately rejected', 'length': 5, 'answers': ['EXTRA']},
             {'clue': 'In revolutionary china, run by Mao, mostly as leader', 'length': 8, 'answers': ['CHAIRMAN']},
             {'clue': 'story of church fellow in charge, having drink, losing head', 'length': 9,
              'answers': ['CHRONICLE']},
             {'clue': 'Liberal in church welcoming church\'s lack of restraint', 'length': 7, 'answers': ['LICENCE']},
             {'clue': 'No longer taking exercise, one in depression needed to get fit', 'length': 9,
              'answers': ['EXPEDIENT']},
             {'clue': 'Naughty word shocked nicest boy', 'length': 9, 'answers': ['OBSCENITY']},
             {'clue': 'Official service covers times gone by', 'length': 8, 'answers': ['MAYORESS']},
             {'clue': 'System of reasoning set up has frantic hospital missing out', 'length': 9,
              'answers': ['DIALECTIC']},
             {'clue': 'Actors\' fees take balls', 'length': 8, 'answers': ['CASTRATE']},
             {'clue': 'cowl without century bird', 'length': 3, 'answers': ['OWL']},
             {'clue': 'decided without police act', 'length': 4, 'answers': ['DEED/OVER']},
             {'clue': 'Fuming having lost silver wreath', 'length': 4, 'answers': ['RING']},
             {'clue': 'depict bad accident without police', 'length': 5, 'answers': ['ENACT']},
             {'clue': 'Badly ported losing short time for junkie', 'length': 5, 'answers': ['DOPER']},
             {'clue': 'First meandering breeze', 'length': 4, 'answers': ['WIND']},
             {'clue': 'Agree to fool recipe, initially', 'length': 7, 'answers': ['CONFORM']},
             {'clue': 'First bash wears acquittal', 'length': 6, 'answers': ['PARDON']},
             {'clue': 'Haggling ends fishing', 'length': 7, 'answers': ['ANGLING']},
             {'clue': 'Flop requesting ends sunbathing', 'length': 7, 'answers': ['BASKING']},
             {'clue': 'Throw party, ending recess', 'length': 5, 'answers': ['STASH']},
             {'clue': 'Encircle swamp - bad in the middle', 'length': 5, 'answers': ['ORBIT']},
             {'clue': 'Lease glum workers inside', 'length': 4, 'answers': ['RENT']},
             {'clue': 'Wine from instrument ends', 'length': 6, 'answers': ['CLARET']},
             {'clue': 'Choose worker\'s extreme measures', 'length': 5, 'answers': ['PINTS']},
             {'clue': 'Prank from concern - terrific extremes', 'length': 5, 'answers': ['CAPER']},
             {'clue': 'Twin egg-shaped extremes of instrument', 'length': 4, 'answers': ['TOOL']},
             {'clue': 'Twin egg-shaped backs extremes of bounty', 'length': 4, 'answers': ['LOOT']},
             {'clue': 'Shop, except spat after going around stores', 'length': 5, 'answers': ['GRASS']},
             ]

# Only method we need at first, a simple solve endpoint
@app.route("/solveClueList", methods=['POST'])
@cross_origin()
def solveClueList():
    headers = request.headers  # For future usage, allows us to pass options through to wrapper?
    try:
        # Pull body from request
        body = request.get_json()
        all_results = []
        clues = body["clues"]
        for c in clues:
            clue = c["clue"]
            length = int(c['length'])
            print(f"Running solver with clue {clue} and length {length}")  # Simple debug message
            # Default empty lists for answers and explanations, to be populated by wrapper output (if applicable)
            results = []
            (straight, sols) = solveClue(clue, length)
            for (a, exp) in sols:
                results.append({"answer": a, "explanation": exp})
            morse_answered = len(results) > 0
            if not morse_answered:
                for elem in test_data:
                    if elem['clue'] == clue.lower():
                        results = [{"answer": elem['answers'][0], "explanation": ""}]
            all_results.append({"clue_text": clue, "clue_length": length, "morse_answered": morse_answered, "results": results})
        return {"results": all_results, "clue_count": len(clues)}, 200
    except Exception as e:
        return {"msg": f"Something went wrong: {e}", "error": 500}, 500

# Only method we need at first, a simple solve endpoint
@app.route("/solve", methods=['POST'])
@cross_origin()
def solve():
    headers = request.headers  # For future usage, allows us to pass options through to wrapper?
    try:
        # Pull body from request
        body = request.get_json()
        clue = body["clue"]
        length = int(body['length'])
        print(f"Running solver with clue {clue} and length {length}")  # Simple debug message
        # Default empty lists for answers and explanations, to be populated by wrapper output (if applicable)
        results = []
        (straight, sols) = solveClue(clue, length)
        for (a, exp) in sols:
            results.append({"answer": a, "explanation": exp})
        morse_answered = len(results) > 0
        if not morse_answered:
            for elem in test_data:
                if elem['clue'] == clue.lower():
                    results = [{"answer": elem['answers'][0], "explanation": ""}]
        return {"results": results, "morse_answered": morse_answered}, 200
    except Exception as e:
        return {"results": [], "morse_answered": False, "msg": f"Something went wrong: {e}", "error": 500}, 500

@app.route("/solveBuiltIn", methods=['POST'])
@cross_origin()
def solveBuiltIn():
    headers = request.headers  # For future usage, allows us to pass options through to wrapper?
    try:
        # Pull body from request
        body = request.get_json()
        clueNum = int(body["clueNum"])
        print(f"Running solver with clue #{clueNum} from haskell lib")  # Simple debug message
        # Default empty lists for answers and explanations, to be populated by wrapper output (if applicable)
        results = []
        (straight, sols) = solveBuiltinClue(clueNum)
        for (a, exp) in sols:
            results.append({"answer": a, "explanation": exp})
        morse_answered = len(results) > 0
        return {"results": results, "morse_answered": morse_answered}, 200
    except Exception as e:
        return {"msg": f"Something went wrong: {e}", "error": 500}, 500


if __name__ == "__main__":
    port = int(os.environ.get('PORT', 5000))
    app.run(host='0.0.0.0', port=port)
