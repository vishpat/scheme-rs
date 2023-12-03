sudo apt update
curl https://apt.llvm.org/llvm-snapshot.gpg.key | \
    sudo tee /etc/apt/trusted.gpg.d/apt.llvm.org.asc
echo "deb http://apt.llvm.org/bullseye/ llvm-toolchain-bullseye-15 main" | sudo tee /etc/apt/sources.list.d/llvm.list
sudo apt-get update
sudo apt-get install --yes git build-essential llvm-15 llvm-15-dev libpolly-15-dev
