import pymongo

CLIENT: pymongo.MongoClient = pymongo.MongoClient("pda-mongo", 27017)
PDA_DB = CLIENT.pda
DESIGNS = PDA_DB.designs
