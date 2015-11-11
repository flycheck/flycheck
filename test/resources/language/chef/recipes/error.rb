# FC002: Avoid string interpolation where not required
package "mysql-server" do
  version "#{node['mysql']['version']}"
  action :install
end

# FC003: Check whether you are running with chef server before using server-specific features
nodes = search(:node, "hostname:[* TO *] AND chef_environment:#{node.chef_environment}")

# FC004: Use a service resource to start and stop services
execute "start-tomcat" do
  command "/etc/init.d/tomcat6 start"
  action :run
end
