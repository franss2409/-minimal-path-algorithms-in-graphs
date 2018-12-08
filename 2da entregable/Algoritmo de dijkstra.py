Python 3.7.1 (v3.7.1:260ec2c36a, Oct 20 2018, 14:57:15) [MSC v.1915 64 bit (AMD64)] on win32
Type "help", "copyright", "credits" or "license()" for more information.
>>> #Creador: Eduardo Javier Maldonado Acevedo
#Carrera: Ingenieria en informatica
import os
import sys

class Dijkstra:
    def __init__(self):
        self.grafo = {'TIJ':{'MTY': 800},
                      'MZT':{'TIJ': 400, 'BJX': 300},
                      'MTY':{'BJX': 700},
                      'BJX':{'SAN': 900, 'TAM': 400, 'MEX': 350},
                      'GDL':{'MZT': 500, 'BJX': 250, 'MEX': 500, 'MTY': 450},
                      'CUN':{'GDL': 650},
                      'MEX':{'CUN': 650, 'MID': 450, 'CH': 50},
                      'TAM':{'MID': 450},
                      'SAN':{'MID': 1200},
                      'MID':{},
                      'CH':{'TAM': 50},
                     }

    def Menu(self):
        print """>============= Algoritmo Dijkstra ==============<
[1] Mostrar Grafo Completo
[2] Algoritmo Dijkstra
[0] Salir
>===========================================<"""

    def seleccion(self):
        while True:
                os.system("cls")
                g.Menu()
                opcion = raw_input(">=> Ingresa tu opcion: ")
                if opcion=="1":
                 g.GrafoCompleto()
                elif opcion=="2":
                 origen = raw_input(">=> Ingresa el origen: ")
                 lladatoe = self.grafo.keys()
                 if origen not in lladatoe:
                    print "El origen no existe"
                    os.system("pause")
                 else:
                    destino = raw_input(">=> Ingresa el destino: ")
                    if destino not in lladatoe:
                        print "El destino no existe"
                        os.system("pause")
                    else:
                       g.MetodoDijkstra(self.grafo,origen,destino)
                elif opcion=="0":
                    break
                else:
                 print "No has ingresado una opcion correcta"
                 os.system("pause")

    def MetodoDijkstra(self,grafo,origen,destino):
        arbol, distancia, padre = {}, {}, {}
        cont = 0
        ordenada = []
        for dato in grafo:
            arbol[dato] = False
            distancia[dato] = sys.maxint
            padre[dato] = ''
        distancia[origen] = 0
        dato = origen
        while dato != destino and arbol[dato] is False:
            arbol[dato] = True
            ady = grafo[dato]
            '''print "Padre:",dato
            print "Adyacentes:",ady
            os.system("pause")'''
            for q, peso in ady.iteritems():
                if distancia[q] > distancia[dato]+peso:
                    distancia[q] = distancia[dato]+peso
                    padre[q] = dato
            dato = min((a for a in grafo if arbol[a] is False), key=lambda a:distancia[a])
            ordenada = distancia.items()
            print "=================================="
            print "Paso:",cont
            print "=================================="
            print "Ordenada:",ordenada
            print "=================================="
            cont += 1
            ordenada = [(a,b) for b,a in ordenada]
            ordenada.sort()
            ordenada = [(a,b) for b,a in ordenada]

        if padre[destino]:
            camino = [destino]
            dato = destino
            print "=========== Resultados ==========="
            while dato != origen:
                camino.append(padre[dato])
                print "Camino:",camino
                #print "Camino:",camino[::-1]
                dato = padre[dato]
            extraido = camino[0]
            for a,b in ordenada:
                if extraido == a:
                    peso = b
            print "Costo:",peso
            print "=================================="
            os.system("pause")
            return camino[::-1]
        else:
            return None

    def GrafoCompleto(self):
        for key, datos in self.grafo.items():
            print key
            print datos
        os.system("pause")

g = Dijkstra()
g.seleccion()
