function(Graph, fuente){
  dist = c()   # VECTOR DE DISTANCIAS
  source = fuente   # NODO FUENTE
  vertex = as.numeric(V(Graph))   # NODOS
  previous = c()   # NODO PREVIO
  alt = c()
  for (i in 1:length(vertex)){# INICIALIZACI???N
    # DISTANCIA INICIAL DESDE LA FUENTE HASTA EL VERTICE V 
    # SE ESTABLECE COMO INFINITO
    dist[i] = Inf
    # NODO PREVIO EN RUTA ???PTIMA DESDE LA FUENTE
    previous[i] = NA
  }
  # DISTANCIA DESDE LA FUENTE HACIA LA FUENTE
  dist[source] = 0
  # EL CONJUNTO DE TODOS LOS NODOS Q EN EL GRAFO
  # TODOS LOS NODOS EN EL GRAFO NO EST???N OPTIMIZADOS
  Q = vertex
  
  # CICLO PRINCIPAL DEL ALGORITMO
  while(length(Q)>0){
    # NODO EN Q CON LA MENOR DISTANCIA
    u = Q[which(dist[Q] == min(dist[Q]))][1]
    # REMOVER u DEL CONJUNTO Q
    Q = Q[-(which(Q==u))]
    
    # HALLAR VECINOS DE u
    vecino = as.numeric(neighbors(g, u))
    # PARA CADA VECINO v DE u (DONDE v NO HA SIDO REMOVIDO DE Q)
    for (i in 1:length(vecino)){
      # C???LCULO DE DISTANCIAS
      alt[i] = dist[u] +
        distances(g, v=u, to=vecino[i])
      # ACTUALIZO DISTANCIA (u,v)
      if (alt[i] < dist[vecino[i]]){
        dist[vecino[i]] = alt[i]
        # OBTENGO EL NODO PREVIO
        previous[vecino[i]] = u
      }
    }
  }         
  
  # BASE DE RESULTADOS
  result = data.frame(Desde=LETTERS[fuente], Hasta=LETTERS[vertex], 
                      Distancia=dist, Predecesor=LETTERS[previous])
  
  # RECONSTRUCCI???N DE RUTAS ???PTIMAS
  # DESDE LA MATRIZ CON LAS DISTANCIAS ???PTIMAS
  ruta = function(DIST, destino){       
    fuente = as.character(DIST[1,1])# NODO FUENTE
    path = destino   # NODO DESTINO
    
    # MIENTRAS LOS NODOS SEAN DIFERENTES
    # BUSCA LOS NODOS PREVIOS
    # RECONSTRUYE LA RUTA ???PTIMA
    # ACTUALIZA EL NUEVO DESTINO
    # FORMATO DE SALIDA DE LA RUTA
    while(destino != fuente){
      previo = as.character(DIST[which(DIST[,2]==destino),4]) 
      path = c(previo, path)
      destino = previo
      path = paste(path, collapse = ' -> ')
    }
    return(path)
  }
  
  # APLICO FUNCI???N DE RECONSTRUCCI???N DE RUTAS ???PTIMAS 
  # PARA TODOS LOS NODOS EN EL GRAFO
  rutas = list()   # INICIALIZO LISTA P/RUTAS
  for (i in 1:nrow(result)){
    rutas[[i]] = ruta(result, as.character(result[i,2]))
  }
  result$Rutas = unlist(rutas)
  result = result[,-4]
  
  return(result)
}

