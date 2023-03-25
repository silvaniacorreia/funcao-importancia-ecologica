# Função Importância Ecológica #

Função para calcular a densidade relativa, a frequência relativa, a dominância relativa, o Índice de Valor de Importância e o Índice de Valor de Cobertura de espécies arbóreas.

## Description: ##

Esta função permite ao usuário a comparação entre comunidades florestais quanto à importância ecológica das espécies que as compõem. Esta comparação poderá ser feita através do Índice de Valor de Importância - IVI, do Índice de Valor de Cobertura - IVC ou de ambos. Além disso, a função gerará diagramas de frequência para cada comunidade florestal estudada. Estes diagramas são histogramas que exibem o número de espécies em cada classe de frequência absoluta (porcentagem de parcelas que apresentam determinada espécie).

## Usage: ##

**eco.imp(x, index = "both", diagram = TRUE, dim.diagram = NULL, save.diagram = FALSE)**

## Arguments: ##

**x**	              
Conjunto de dados de entrada. Deve ser um dataframe.

**index**			      
Índice que deve ser calculado. Pode-se escolher "both", "ivi" ou "ivc".

**diagram**			    
Define se deverão ser gerados diagramas de frequência absoluta para cada comunidade florestal estudada. Pode ser TRUE ou FALSE.

**dim.diagram**	    
Obrigatório quando diagram = TRUE. Define em quantas linhas e colunas os diagramas gerados serão distribuídos. A entrada deve ser um vetor numérico contendo o número de linhas e o número de colunas, respectivamente.

**save.diagram**	  
Caso TRUE, salva o diagrama gerado em um arquivo de imagem com formato jpeg no diretório de trabalho. 

## Details: ##

O dataframe de entrada no argumento x deve conter colunas com as informações coletadas em campo, na seguinte ordem:
	1ª coluna: identificação dos locais/comunidades de coleta de dados.
	2ª coluna: identificação das parcelas amostradas.
	3ª coluna: identificação das espécies arbóreas e quantidade de indivíduos amostrados.
	4ª coluna: Diâmetro a altura do peito de cada indivíduo arbóreo amostrado.
Caso index = "ivi", será calculado o Índice de Importância Ecológica das espécies. Para index = "ivc", será calculado o Índice de Valor de Cobertur das espécies. Caso index = "both", ambos os índices serão calculados.

## Value: ##

A função retorna uma lista contendo uma matriz para cada comunidade/local estudado. 
Cada matriz conterá a densidade relativa, a frequência relativa, a dominância relativa, o Índice de Valor de Importância e/ou o Índice de Valor de Cobertura das espécies do local.

## Author(s): ##

Silvania Goularte Correia (silvania.goularte@gmail.com)

## References: ##

Curtis, J. T.; McIntoshi, R. P. An Upland Forest Continuum in the Prairie-Forest Border Region of Wisconsin. Ecology, 32(1): 476-496, 1951.
Instituto Ambiental do Paraná. Parâmetros Fitossociológicos. Disponível em: <http://www.iap.pr.gov.br/arquivos/File/Plano_de_Manejo/PE_Rio_Guarani/Anexos/anexo_2.pdf>. Acesso em: 02 abr. 2015.

## Examples: ##
Faça download do arquivo exemplo.csv.

tabela = read.table("exemplo.csv", header=TRUE, sep=";")

eco.imp(tabela, "both", TRUE, c(2,3), TRUE) #Gera matrizes contendo os índices solicitados e salva os diagramas de frequência absoluta solicitados.

eco.imp(tabela, "both", TRUE, c(2,3), FALSE) #Gera matrizes contendo os índices solicitados e exibe na tela os diagramas de frequência absoluta solicitados.
