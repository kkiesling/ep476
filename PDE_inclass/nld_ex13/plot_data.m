%function plot_data
clear all; clc;

load temperature.dat
load time.dat
load xmesh.dat

nt=length(time);
nx=length(xmesh);

time_data=zeros(nt,nx);
xmesh_data=zeros(nt,nx);

for i=1:nt
    time_data(i,:)=time(i);
end

for i=1:nx
    xmesh_data(:,i)=xmesh(i);
end

mesh(time_data,xmesh_data,temperature)
xlabel('time')
ylabel('x')
zlabel('temp')
