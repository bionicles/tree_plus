provider "aws" {
  region = "us-west-2"
}

resource "aws_instance" "example" {
  ami           = "ami-0c94855ba95c574c8"
  instance_type = "t2.micro"
}

data "aws_ami" "ubuntu" {
  most_recent = true
  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-focal-20.04-amd64-server-*"]
  }
  owners = ["099720109477"] # Canonical
}

variable "instance_type" {
  description = "The instance type to launch."
  default     = "t2.micro"
}

output "instance_public_ip" {
  value = aws_instance.example.public_ip
}

locals {
  service_name = "myservice"
}

module "vpc" {
  source = "terraform-aws-modules/vpc/aws"
  version = "2.77.0"
  name = "my-vpc"
  cidr = "10.0.0.0/16"
  azs  = ["us-west-2a", "us-west-2b", "us-west-2c"]
  public_subnets  = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]
}