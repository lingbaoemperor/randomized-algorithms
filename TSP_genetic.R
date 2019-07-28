
#求某个编码路径长度.
weight <- function(population,starting_point){
  
  #结果
  res = c()
  
  for (j in 1:ncol(population)){
    
    #路径索引,(4,3,2,1)
    path = population[2:nrow(population),j]
    #起始路径.
    node = starting_point
    result = 0
    
    #结点顺序.
    seq = c(1,2,3,4,5)
    tmp = setdiff(seq,starting_point)
    seq = seq[tmp]
    
    for (i in 1:length(seq)){
      #下一个结点索引.
      index = path[i]
      #下一个结点.
      next_node = seq[index]
      result = result+graph[node,next_node]
      node = next_node
      
      if (length(seq) != 1){
      index_tmp = 1:length(seq)
      index_tmp = setdiff(index_tmp,index)
      seq = seq[index_tmp]
      }
    }
    res[j] = result+graph[node,starting_point]
  }
  return(res)
}

#初始种群.
generate_population <- function(number_of_nodes,starting_point){
  pop = array(dim=c(5,100))
  pop[1,] = rep(starting_point,populations)
  for(i in 2:number_of_nodes){
    pop[i,] = ceiling(runif(100,0,number_of_nodes-i+1))
  }
  return(pop)
}
#交叉算子.(单点交叉)
crossover <- function(population){
  n = ncol(population)
  len = nrow(population)
  #随机配对.
  pair_index = sample(1:n,n,FALSE)
  
  for (i in 1:(n/2)){
    parent_1 = population[,i*2-1]
    parent_2 = population[,i*2]
    
    #交叉概率.
    rate = runif(1,0,1)
    if (rate < crossover_rate){
      #交叉位置.
      pos = ceiling(runif(1,0,len-1))
      population[,i*2-1] = c(parent_1[1:pos],parent_2[(pos+1):len])
      population[,i*2] = c(parent_2[1:pos],parent_1[(pos+1):len])
    }
  }
  return(population)
}

#变异算子.(单点变异)
mutation <- function(population){
  n = ncol(population)
  len = nrow(population)
  for (i in 1:n){
    rate = runif(1,0,1)
    if(rate < mutation_rate){
      #变异位置.单点.
      pos = ceiling(runif(1,1,len))
      population[pos,i] = ceiling(runif(1,0,len-pos+1));
    }
  }
  return(population)
}

#选择算子.(父代子代中选择最优的前n个)
selection <- function(population,starting_point){
  
  #适应度函数.
  Fx = sum(graph)-weight(population,starting_point)
  #适应度.
  fit_rate = Fx/sum(Fx)
  
  index = 1:ncol(population)
  tmp = rbind(index,fit_rate)
  tmp = tmp[,order(tmp[2,],decreasing = T)]
  
  #取前populations个.
  
  #平均适应函数值.
  print(mean(Fx[tmp[1,1:populations]]))
  
  return(population[,tmp[1,1:populations]])
}


#参数.
#完全图.5个结点.
graph = array(c(0,3,7,2,9,3,0,5,6,8,7,5,0,1,10,2,6,1,0,4,9,8,10,4,0),c(5,5))
#结点数.
nodes = 5

#编码[A,B,C,D,E]~[1,2,3,4,5]
#起始节点:A~1
starting_node = 1

#种群数.
populations = 100
#交叉概率.
crossover_rate = 0.7
#变异概率.
mutation_rate = 0.1

#初始化种群.
parent <- generate_population(nodes,starting_node)

#保存后代.
new_generation =array()

#迭代次数.
iterator = 20

for (gen in 1:iterator){
  #交叉.变异.
  children = mutation(crossover(parent))
  #选择后代作为下一代父母.
  new_generation = selection(cbind(parent,children),starting_node)
  parent = new_generation
}