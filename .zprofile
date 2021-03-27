## Golang setup
export GOROOT="${HOME}/workspace/tools/golang/go"
export GOPATH="${HOME}/workspace/tools/golang/workspace"
export PATH="${GOROOT}/bin:$PATH"

## Rust (rustup)
source "$HOME/.cargo/env"

## JDK (corretto)
export JAVA_HOME="${HOME}/workspace/tools/corretto/jdk"
export PATH="${JAVA_HOME}/bin:${PATH}"

## Maven
export M2_HOME="${HOME}/workspace/tools/maven/mvn"
export PATH="${M2_HOME}/bin:${PATH}"