
clear

% For empirical simulation:
% Let west be the first location, east be the second
alpha = cell(1,3);
alpha{1,1} = [0.819924,0.180076;0.2166706,0.7833294];
alpha{1,2} = [0.8485627,0.1514373;0.1932759,0.8067241];
alpha{1,3} = [0.8408461,0.1591539;0.2330516,0.7669484];
p = [0.2044,0.2353;0.2489,0.1325]*4;
s = [0.7,1.5;1.5,0.7];
h = [1;1]; % holding cost
% Satisfying (h(2)+s(2,1))*alpha(1,2) <= p_bar(1)
%            (h(1)+s(1,2))*alpha(2,1) <= p_bar(2)

T = 3;
% Weekday demand
mu = [12.0155556,31.251111,22.94;7.8088889,29.613333,16.53778];
sigma = [6.309528,26.38921,22.50818;5.080853,19.10127,13.728];


N = 178; % total number of vehicles
%C = 200; % Capacity at each station

cap_ratio = 0.1:0.1:1;
capacity = cap_ratio*N;

[upper_dockless,lower_dockless,value_dockless] = two_region(alpha,p,s,T,mu,N);

upper_dock = zeros(T,numel(cap_ratio));
lower_dock = zeros(T,numel(cap_ratio));
value_dock = zeros(N+1,numel(cap_ratio));

for i=1:numel(cap_ratio)
    [upper_dock(:,i),lower_dock(:,i),value_dock(:,i)] = two_region_dock(alpha,p,s,h,T,mu,N,capacity(i));
end

for j=1:T
    figure(j);
    plot(cap_ratio,upper_dockless(j)*ones(1,numel(cap_ratio)),cap_ratio,lower_dockless(j)*ones(1,numel(cap_ratio)));
    hold on;
    plot(cap_ratio,upper_dock(j,:),cap_ratio,lower_dock(j,:));
    hold off;
end

figure(T+1);
plot(cap_ratio,mean(value_dockless)*ones(1,numel(cap_ratio)),cap_ratio,mean(value_dock,1));

% figure(1);
% plot(cap_ratio,upper_dockless(1)*ones(1,numel(cap_ratio)),cap_ratio,lower_dockless(1)*ones(1,numel(cap_ratio)));
% hold on;
% plot(cap_ratio,upper_dock(1,:),cap_ratio,lower_dock(1,:));
% hold off;
% 
% figure(2);
% plot(cap_ratio,upper_dockless(2)*ones(1,numel(cap_ratio)),cap_ratio,lower_dockless(2)*ones(1,numel(cap_ratio)));
% hold on;
% plot(cap_ratio,upper_dock(2,:),cap_ratio,lower_dock(2,:));
% hold off;
% 
% figure(3);
% plot(cap_ratio,upper_dockless(3)*ones(1,numel(cap_ratio)),cap_ratio,lower_dockless(3)*ones(1,numel(cap_ratio)));
% hold on;
% plot(cap_ratio,upper_dock(3,:),cap_ratio,lower_dock(3,:));
% hold off;
% 
% figure(4);
% plot(cap_ratio,upper_dockless(4)*ones(1,numel(cap_ratio)),cap_ratio,lower_dockless(4)*ones(1,numel(cap_ratio)));
% hold on;
% plot(cap_ratio,upper_dock(4,:),cap_ratio,lower_dock(4,:));
% hold off;