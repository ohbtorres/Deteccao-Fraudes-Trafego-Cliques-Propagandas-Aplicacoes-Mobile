# Detecção de Fraudes em Tráfego de Cliques em Propagandas de Aplicacoes Mobile
Modelo de aprendizado de máquina para determinar se um clique é fraudulento ou não.

Projeto executado a partir do dataset disponibizado na competição do Kaggle: https://www.kaggle.com/c/talkingdata-adtracking-fraud-detection/data

# Descrição dos arquivos

train_sample.csv - amostra dos dados de treino (a base completa pode ser obtida no link acima do Kaggle)

test.csv - dados de teste

# Campos dados de treino
ip: endereço de ip do click.

app: app id.

device: id do tipo de  do usuário dispositivo (ex.: iphone 6 plus, iphone 7, huawei mate 7, etc.)

os: versão do Sistema Operacional

channel: id do canal do dispositivo móvel

click_time: data e hora do click (UTC)

attributed_time: se o usuário fez um download no app, após ter realizado um clique em um anúncio, este é o tempo do download

is_attributed: variável a ser predita, dizendo se houve ou não download
