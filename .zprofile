## Golang setup
export GOROOT="${HOME}/workspace/tools/golang/go"
export GOPATH="${HOME}/workspace/tools/golang/workspace"
export PATH="${GOROOT}/bin:$PATH"


## Rust setup
source "$HOME/.cargo/env"


## Java setup
export JAVA_HOME="${HOME}/workspace/tools/java/amazon-corretto-15.0.1.9.1-linux-x64"
export PATH="${JAVA_HOME}/bin:$PATH"

export M2_HOME="${HOME}/workspace/tools/maven/apache-maven-3.6.3"
export PATH="${M2_HOME}/bin:$PATH"


## Add local bin folder to PATH
export PATH="${HOME}/.local/bin:$PATH"
