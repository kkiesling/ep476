function main
clear; clc; close all;

% bc=0, ic=0, s=1, implictit and explicit
load temperatureSE.dat
load temperatureSI.dat
load timeS.dat
load xmeshS.dat
plot_data_gen(temperatureSE, temperatureSI, xmeshS, timeS)



function plot_data_gen(temperature1, temperature2, xmesh, time)
format long

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
