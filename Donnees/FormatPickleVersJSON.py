import pickle
from json import JSONEncoder
import json
import numpy
class NumpyArrayEncoder(JSONEncoder):
    def default(self, obj):
        if isinstance(obj, numpy.ndarray):
            return obj.tolist()
        return JSONEncoder.default(self, obj)

def ouvrirEtEnregistrerSousUnAutreFormat():
    f = open("mnist.pkl", "rb") 
    tr_d, va_d, te_d = pickle.load(f, encoding='latin1')
    
    file = open("traningData.json", "w")
    tabImage, chiffreCorrespondant = tr_d
    LListeDIcoTraining = []
    for i in range(chiffreCorrespondant.size):
        LListeDIcoTraining.append({str(chiffreCorrespondant[i]) : tabImage[i]})

    json.dump(LListeDIcoTraining, file, indent="", cls=NumpyArrayEncoder)

    LListeDIcoEvaluate = []
    file = open("evaluateData.json", "w")
    tabImage, chiffreCorrespondant = va_d
    for i in range(chiffreCorrespondant.size):
        LListeDIcoEvaluate.append({str(chiffreCorrespondant[i]) : tabImage[i]}) 

    json.dump(LListeDIcoEvaluate, file, indent="", cls=NumpyArrayEncoder)

    LListeDIcoTest = []   
    file = open("testData.json", "w")
    tabImage, chiffreCorrespondant = te_d
    for i in range(chiffreCorrespondant.size):
        LListeDIcoTest.append({str(chiffreCorrespondant[i]) : tabImage[i]}) 

    json.dump(LListeDIcoTest, file, indent="", cls=NumpyArrayEncoder)
    
