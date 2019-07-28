#每一蚂蚁,某个概率选择一个城市，移动到该城市,更新m个蚂蚁的禁忌城市表,更新路径上的信息素数量，更新每个城市的行进概率.

#完全图.
graph = array(c(0,3,7,2,9,3,0,5,6,8,7,5,0,1,10,2,6,1,0,4,9,8,10,4,0),c(5,5))
#结点数.
nodes = 5
#蚂蚁数.
m = 10
#起始位置.
starting_node = 1
#m个蚂蚁的禁忌城市表.nodes*m.
route = array(rep(0,nodes*m),c(nodes,m))
#m个蚂蚁当前的位置.初始A~1.
pos = array(rep(1,m))
#城市的能见度.
visibility = 1/graph
#信息素初始强度.
concentrations = 1
#每条路的信息素强度.初始1.
pheromones = array(rep(concentrations,nodes^2),c(nodes,nodes))

#每次迭代后信息素的增量.
increment = array(rep(0,nodes^2),c(nodes,nodes))

#初始化
init <- function(){
  #清空禁忌表.
  route[,] <<- 0
  
  #设置起始位置.
  pos[] <<- starting_node
  route[1,] <<- starting_node
  
  #清空信息素.
  pheromones[,] <<- array(rep(concentrations,nodes^2),c(nodes,nodes))
  #清空信息素增量.
  increment = array(rep(0,nodes^2),c(nodes,nodes))
}

#移动蚂蚁.参数：蚂蚁编号，当前时间.
move <- function(ant,p){
  
  #该蚂蚁当前所在城市,pos[ant]
  city_now = pos[ant]
  #所有可移动城市.
  movable_cities = setdiff(1:nodes,route[,ant])
  #可以移动的城市有nodes-p+1个.
  
  #城市编号和移动概率..
  res = matrix(nrow=length(movable_cities),ncol=2)
  len = length(movable_cities)
  
  city = -1
  if(len != 1){
    #计算剩余x个城市的概率.
    total = 0
    for (i in 1:len){
      city_next = movable_cities[i]
      tmp = visibility[city_now,city_next]*pheromones[city_now,city_next]
      total = total + tmp
      res[i,1] = city_next
      res[i,2] = tmp
    }
    #城市移动概率.
    res[,2] = res[,2]/total
    
    #转化为累计概率.
    for (i in 2:len){
      res[i,2] = res[i,2]+res[i-1,2]
    }
    #概率移动到某个城市.
    prob = runif(1)
    for (i in 1:len){
      if(prob <= res[i,2]){
        city = res[i,1]
        break
      }else{}
    }
  }else{
    #只有一个城市，一定移动到这个城市.
    city = movable_cities[1]
    }
  #该城市加入禁忌表.
  route[p,ant] <<- city
  #更新蚂蚁当前位置.
  pos[ant] <<- city
}

#m个蚂蚁中的最优路径长度.
best_way <- function(){
  min_weight = 999999
  #m条路径.
  for(i in 1:m){
    circle = c(route[,i],starting_node)
    tmp = 0
    for(j in 1:nodes){
      k = circle[j]
      kk = circle[j+1]
      tmp = tmp + graph[k,kk]
    }
    min_weight = min(tmp,min_weight)
  }
  return(min_weight)
}

###entry###

t = 1     #当前时间.
iterator = 10  #迭代次数.

#初始化.
init()
for (it in 1:iterator){
  #每次迭代,走完剩余n-1个城市.
  for (n in 2:nodes){
    #时间增加.
    t = t+1
    #对于每个城市，遍历每个蚂蚁.
    for (ant in 1:m){
      #该蚂蚁以某个概率移动到下一个城市（没在禁忌表中的城市）.
      move(ant,n)
    }
    # print(route)
    #更新该次经过的路径上的信息素.（蚁密）
    pheromones = pheromones + increment
    increment[,] = 0
  }
  #计算当前最优路径.
  print(best_way())
  #一次迭代后,清除所有信息，蚂蚁回到起始位置.
  init()
}