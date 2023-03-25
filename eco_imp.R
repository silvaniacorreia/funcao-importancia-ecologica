setwd("D:/Documentos/Mestrado/R/trabalho final")
tabela = read.table("exemplo.csv", header=TRUE, sep=";")

###### Funcao importancia ecologica ######

eco.imp = function(x, index = "both", diagram=TRUE, dim.diagram = c(2,2), save.diagram = FALSE, dim.jpeg = c(1000, 1000), res.jpeg = 72) #criacao da funcao e seus argumentos
	{
	##Validacao dos argumentos
	if (diagram != TRUE & diagram != FALSE) #verifica se o usuario inseriu um valor logico para o argumento "diagram"
		{
		stop("Invalid diagram") #exibe mensagem de erro caso o usuario nao tenha inserido um valor logico em "diagram"
		}
	if(diagram) #caso o usuario queira que diagramas sejam gerados, serao realizadas as validacoes abaixo
		{
		if(length(dim.diagram) != 2 | class(dim.diagram) != "numeric") #verifica se o usuario inseriu um vetor de duas posicoes da classe "numeric" em "dim.diagram"
			{
			stop("Invalid dim.diagram") #exibe mensagem de erro caso o usuario nao tenha inserido um vetor de duas posicoes da classe "numeric" em "dim.diagram"
			}
		if(save.diagram != TRUE & save.diagram != FALSE) #verifica se o usuario inseriu um valor logico para o argumento "save.diagram"
			{
			stop("Invalid save.diagram") #exibe mensagem de erro caso o usuario nao tenha inserido um valor logico em "save.diagram"
			}
		if(save.diagram)
			{
				if(length(dim.jpeg) != 2 | class(dim.jpeg) != "numeric") #verifica se o usuario inseriu um vetor de duas posicoes da classe "numeric" em "dim.jpeg"
				{
				stop("Invalid dim.jpeg") #exibe mensagem de erro caso o usuario nao tenha inserido um vetor de duas posicoes da classe "numeric" em "dim.jpeg"
				}
				if(length(res.jpeg) != 1 | class(res.jpeg) != "numeric")#verifica se o usuario inseriu um vetor de uma posicao da classe "numeric" em "res.jpeg"
				{
				stop("Invalid res.jpeg") #exibe mensagem de erro caso o usuario nao tenha inserido um vetor de uma posicao da classe "numeric" em "res.jpeg"
				}
			}
		}
	if (index != "both" & index != "ivi" & index != "ivc") #verifica se o usuario inseriu um nome valido para "indice"
		{
		stop("Invalid Index") #exibe mensagem de erro caso o usuario nao tenha inserido um nome valido em "indice"
		}

	##Declaracao de variaveis
	n.species = table(x[,3], x[,1]) #cria tabela com o numero de individuos de cada especie por local estudado
	total = apply(n.species, 2, sum) #cria objeto obtendo o numero total de individuos presentes em cada local estudado
	dr = n.species #cria objeto que contera a densidade relativa de cada especie por local estudado
	for(i in 1:dim(x)[2]) #abre ciclo para calcular densidade relativa de cada especie por local estudado
		{
		dr[,i]=(n.species[,i]/total[i])*100 #calculo de densidade relativa de cada especie por local estudado
		}
	n.parcel = x #atribui o data.frame contido em "x" para outro objeto
	n.parcel[,1] = as.factor(n.parcel[,1]) #converte a coluna 1 de n.parcel (que contem o nome dos locais estudados) em objeto da classe fator
	n.site = length(unique(n.parcel[,1])) #cria um objeto contendo o numero de locais estudados
	uni = unique(n.parcel[,1]) #cria objeto contendo o nome de cada local estudado
	fr = c() #cria objeto que contera a frequencia relativa de cada especie por local estudado
	fa = c() #cria objeto que contera a frequencia absoluta de cada especie por local estudado
	sp.site = c() #cria objeto que contera o numero de especies presentes em cada local estudado
	area.b = x #atribui o data.frame contido em "x" para outro objeto
	area.b$area_basal = pi*(area.b[,4]/2)^2 #cria uma coluna no data.frame contendo a area basal de cada arvore amostrada
	tabela.area = aggregate(area.b$area_basal, by=list(area.b[,1], area.b[,3]), sum) #cria objeto contendo a area basal total de cada especie por local estudado
	dor = c() #cria objeto que contera a dominancia relativa de cada especie por local estudado
	niveis = levels(unique(x[,3])) #cria objeto contendo o nome de todas as especies presentes no data.frame de "x"
	resultados = new.env() #cria um environment que contera uma matriz para cada local estudado, contendo DR, FR, DoR e os indices solicitados pelo usuario das especies 
	
	##Calculo de DR, FR, FA e DoR
	for(i in 1:n.site) #abre ciclo para o calculo de DR, FR e DoR em cada local estudado
		{
		##calculo de frequencia relativa de especie por local estudado
		tabela.parcel = n.parcel[n.parcel[,1] == uni[i],] #cria objeto contendo parcelas, especies e dbh de determinado local estudado
		tabela2 = table(tabela.parcel[,3], tabela.parcel[,2]) #cria objeto contendo as parcelas de determinado local em que cada especie foi amostrada
		multip = dim(tabela2)[1]*dim(tabela2)[2] #cria objeto contendo o numero de itens presentes na tabela2
		for(j in 1:multip) #abre ciclo para inserir "1" em cada parcela onde cada espécie está presente
			{
			if(tabela2[j] != 0) #verifica se há espécie presente na parcela
				{
				tabela2[j] = 1 #insere "1" caso haja especie presente na parcela
				}
			}
		tabela3 = apply(tabela2,1,sum) #cria objeto contendo o numero de parcelas em que cada especie esta presente
		tabela4 = data.frame(tabela3, rep(uni[i], length(tabela3))) #cria um data.frame contendo o numero de parcelas em que cada especie esta presente e uma coluna identificando o local de origem destas especies
		parcelas = length(unique(tabela.parcel[,2])) #contabiliza o total de parcelas presentes em determinado local
		tabela4[,1] = tabela4[,1]/parcelas #calcula a frequencia absoluta de cada especie em determinado local
		tabela5 = tabela4[,1]*100 #tranforma a frequencia absoluta em porcentagem e atribui para um novo objeto
		soma.freq = sum(tabela4[,1]) #soma as frequencias absolutas de todas as especies
		tabela4[,1] = (tabela4[,1]/soma.freq)*100 #calcula a frequencia relativa de cada especie em determinado local
		fr = c(fr, tabela4) #cria objeto contendo a FR calculada em todos os locais
		fa = c(fa, tabela5) #cria objeto contendo a FA calculada em todos os locais
		sp = length(unique(tabela.parcel[,3])) #contabiliza o numero de especies presentes em determinado local
		sp.site = c(sp.site, sp) #objeto contendo o numero total de especies presentes em cada um dos locais estudados
		tabela.area1 = tabela.area[tabela.area$Group.1 == uni[i],] #cria objeto contendo a area basal das especies de determinado local estudado
		tabela.area1$x = (tabela.area1$x/sum(tabela.area1$x))*100 #cria coluna contendo a DoR das especies de determinado local estudado
		dor = c(dor, tabela.area1) #cria objeto contendo a DoR calculada em todos os locais
		novo = rep(NA,length(unique(x[,3]))) #cria objeto que contera a DoR das especies que estao presentes em determinado local e o valor "0" para as que não estão presentes (para possibilitar a montagem da matriz final)
		n = 1 #cria contador
		faltantes = length(unique(x[,3]))-length(dor[i*3-1]$Group.2) #calcula o numero de especies faltantes em cada local (considerando o numero total de especies em todos os locais)
		dor[i*3-1]$Group.2 = as.vector(dor[i*3-1]$Group.2) #converte o fator contendo as especies de cada local em um vetor
		dor[i*3-1]$Group.2 = c(dor[i*3-1]$Group.2, rep("null", faltantes)) #insere a palavra "null" em numero de vezes correspondente a quantidade de especies faltantes
		for(j in 1:length(unique(x[,3]))) #abre ciclo para inserir "0" como DoR das especies que nao estao presentes no local
			{
			if(niveis[j] != dor[i*3-1]$Group.2[n]) #caso a especie nao esteja presente no local
				{
				novo[j] = 0 #insere um 0 no vetor caso a especie nao esteja presente no local
				}
			else #caso a especie esteja presente no local
				{
				novo[j] = dor[i*3]$x[n] #insere a sua DoR correspondente no vetor
				n = n+1 #soma 1 no contador para mudar a posicao de comparacao do "if"
				}
			}
		dor[i*3]$x = novo #atribui o conteudo do vetor contendo zeros para as especies faltantes do local estudado ao objeto contendo a DoR de todas as especies em todos os locais
		local1 = matrix(c(dr[,i], fr[i*2-1]$tabela3, dor[i*3]$x), ncol = 3, nrow = length(unique(x[,3])), dimnames = list(rownames(dr), c("DR (%)", "FR (%)", "DoR (%)"))) #cria matriz contendo a DR, FR e DoR de determinado local
	 
		##Calculo dos indices IVI e/ou IVC
		if(index == "both" | index == "ivi") #verifica se o usuario selecionou both ou ivi em "index"
			{
			local1.ivi = apply(local1, 1, sum) #calcula o ivi das especies de determinado local
			local1.ivi.por = local1.ivi/3 #cria um objeto contendo o ivi relativo de cada especie
			}
		if (index == "both" | index == "ivc") #verifica se o usuario selecionou both ou ivc em "index"
			{
			local1.ivc = apply(local1, 1, sum)-local1[,2] #calcula o ivc das especies de determinado local
			local1.ivc.por = local1.ivc/2 #cria um objeto contendo o ivc relativo de cada especie
			}
		if (index == "both") #verifica se o usuario selecionou both para "index"
			{
			local2 = matrix(c(dr[,i], fr[i*2-1]$tabela3, dor[i*3]$x, local1.ivi, local1.ivi.por, local1.ivc, local1.ivc.por), ncol = 7, nrow = length(unique(x[,3])), dimnames = list(rownames(dr), c("DR (%)", "FR (%)", "DoR (%)", "IVI", "IVIR (%)", "IVC", "IVCR (%)"))) #cria matriz contendo DR, FR, DoR e ambos os indices para cada especie de determinado local
			} 
		else if (index == "ivi") #caso o usuario queira somente o ivi
			{
			local2 = matrix(c(dr[,i], fr[i*2-1]$tabela3, dor[i*3]$x, local1.ivi, local1.ivi.por), ncol =5, nrow = length(unique(x[,3])), dimnames = list(rownames(dr), c("DR (%)", "FR (%)", "DoR (%)", "IVI", "IVIR (%)"))) #cria matriz contendo DR, FR, DoR e IVI das especies de determinado local
			} 
		else #caso o usuario queira somente o ivc
			{
			local2 = matrix(c(dr[,i], fr[i*2-1]$tabela3, dor[i*3]$x, local1.ivc, local1.ivc.por), ncol = 5, nrow = length(unique(x[,3])), dimnames = list(rownames(dr), c("DR (%)", "FR (%)", "DoR (%)", "IVC", "IVCR (%)"))) #cria matriz contendo DR, FR, DoR e IVC das especies de determinado local
			}
		assign(paste(uni[i]), local2, envir = resultados) #atribui a matriz contendo todos os indices calculados no environment criado anteriormente
		}
	matrizes = as.list(resultados) #cria uma lista com as matrizes contidas no environment 
	
	##Geracao dos diagramas de frequencia absoluta
	if(diagram) #se o usuario optou por obter diagramas de frequencia absoluta das especies em cada local
		{
		if(save.diagram) #se o usuario optou por salvar os diagramas criados no diretorio de trabalho
			{
			jpeg(filename = "frequency_diagram.jpg", width = dim.jpeg[1], height = dim.jpeg[2], units = "px", pointsize = 12, quality = 100, bg = "white",  res = res.jpeg) #abre dispositivo jpeg para salvar os diagramas
			}
		s = 1 #cria contador 
		par(mfrow=c(dim.diagram[1],dim.diagram[2])) #permite construir uma figura com mais de um diagrama (com numero de linhas e colunas fornecidos pelo usuario)
		for(q in 0:(n.site-1)) #abre ciclo para contruir um diagrama de FA para cada local
			{
			n.sp.site = fa[s:(s+(length(unique(x[,3])))-1)] #cria objeto contendo a frequencia absoluta das especies em determinado local
			n.sp.site1 = n.sp.site[n.sp.site!=0] #retira as especies que possuem FA igual a 0 (ou seja, nao estao presentes no local)
			hist(n.sp.site1, main = paste("Frequency Diagram - Location:" , uni[q+1]), col="black", border = "white", breaks = c(0, 20, 40, 60, 80, 100), xlab = "Absolute frequency (%)", ylab = "Number of species") #cria histograma de frequencia absoluta das especies
			s = s+length(unique(x[,3])) #soma um valor ao contador
			}
		if(save.diagram) #caso o usuario tenha optado por salvar o diagrama em um arquivo de imagem
			{
			dev.off() #fecha o dispositivo de imagem aberto
			}
		}
	return(matrizes) #retorna as matrizes de cada local com os indices solicitados (resultado da funcao)
	}
##fim

