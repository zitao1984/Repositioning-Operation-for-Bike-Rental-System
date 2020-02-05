function [upper,lower,opt_val] = two_region(alpha,p,s,T,mu,N)

%alpha = [0.87,0.13;0.72,0.28];
%p = [15.34,12.58;15.05,9.6];
%s = [13.81,11.33;13.55,8.63];

%T = 4;
mu1 = mu(1,:);
mu2 = mu(2,:);
d = cell(2,T);
for k=1:T
    d{1,k} = floor(0.5*mu1(k)):floor(1.5*mu1(k));
    d{2,k} = floor(0.5*mu2(k)):floor(1.5*mu2(k));
end
% mu1 = mu(1);
% mu2 = mu(2);
% d_1 = 0.5*mu1:1.5*mu1;
% d_2 = 0.5*mu2:1.5*mu2; % Uniform distribution
%N = 300; % total number of vehicles

%p_bar = sum(alpha.*p,2); % average lost sales cost
p_bar = zeros(2,T);
for k=1:T
    p_bar(:,k) = sum(alpha{1,k}.*p,2); % average lost sales cost
end

% state space: x=0-N
action = zeros(N+1,T); % level after repositioning (y)
value = zeros(N+1,T+1);

for t=T:-1:1
    for x=0:N
        re=x-N:x; % possible repositioning decision
        re_cost = zeros(1,numel(re));
        re_cost(re<0) = -re(re<0)*s(2,1);
        re_cost(re>=0) = re(re>=0)*s(1,2); % repositioning cost
        
        y = x*ones(1,numel(re)) - re; % inventory level after repositioning
        E_J = zeros(1,numel(re)); % expected cost to go
        for dec=1:numel(re)
            for i=1:numel(d{1,t})
                for j=1:numel(d{2,t})
                    w1 = min(y(dec),d{1,t}(i));
                    w2 = min(N-y(dec),d{2,t}(j)); % accepted demand
                
                    x_next = y(dec)-floor(alpha{1,t}(1,2)*w1)+floor(alpha{1,t}(2,1)*w2);
%                     if t==1
%                         %error = [t,x,dec,i,j,x_next]
%                     end
                    E_J(dec) = E_J(dec) + p_bar(1,t)*(d{1,t}(i)-w1) + p_bar(2,t)*(d{2,t}(j)-w2) + value(x_next+1,t+1);
                end
            end
        end
        
        [value(x+1,t),action_index] = min(re_cost+E_J*(1/numel(d{1,t}))*(1/numel(d{2,t})));
        action(x+1,t) = y(action_index);
    end
end
upper = action(end,:)';
lower = action(1,:)';
opt_val = value(:,1);
end