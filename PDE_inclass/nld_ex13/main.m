function main

end function
function plot_data_gen(temperature1.dat, temperature2.dat, xmesh.dat, time.dat)
clear all; clc;

format long

load temperature1.dat
load temperature2.dat
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

temp_diff=(temperature1-temperature2)./temperature1;

figure
subplot(1,2,1)
mesh(time_data,xmesh_data,temperature1)
xlabel('time')
ylabel('x')
zlabel('temperature')

subplot(1,2,2)
mesh(time_data,xmesh_data,temperature2)
xlabel('time')
ylabel('x')
zlabel('temperature')

figure
mesh(time_data,xmesh_data,temp_diff)
xlabel('time')
ylabel('x')
zlabel('temperature')

% plot 2d of last time val
figure
plot(xmesh_data(nt,:),temperature1(nt,:),xmesh_data(nt,:),temperature2(nt,:))
xlabel('x')
ylabel('temperature')
end function