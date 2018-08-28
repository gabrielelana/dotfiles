alias ew="cd /opt/projects/Easywelfare/"
alias ewpo="cd /opt/projects/Easywelfare/mvrs-servizi-muoversiservizi/"
alias ewpa="cd /opt/projects/Easywelfare/payroll/"

alias shrink-elasticsearch="sudo systemctl stop elasticsearch-fluentd.service && sudo sed -i -e 's/Xms512m/Xms256m/' /etc/elasticsearch/fluentd/jvm.options && systemctl start elasticsearch-fluentd.service"

source /etc/profile.d/easywelfare_env.sh
source /etc/profile.d/rvm.sh
