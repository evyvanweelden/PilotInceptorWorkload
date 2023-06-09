%% %%%% PILOT INCEPTOR WORKLOAD %%%% %%

% By Evy van Weelden and Carl van Beek, Tilburg University
% Contact: e.vanweelden@tilburguniversity.edu
% Last updated on: 09-06-2023
% Script is based on equations from: I. Niewind, "Pilot Gain and the Workload Buildup Flight Test Technique: A Closer Investigation of Pilot Inceptor Workload", DLR-Interner Bericht, Report No. IB 111-2012/74, Oct. 2012, URL: https://elib.dlr.de/88208/. 

%% Store control input data in cells.

logs = dir('*.h5'); % our input was .h5 files % change accordingly %

% Add your data to cell structure:
stick_VR = { };    % each cell must correspond to one trial/log file % each cell must contain a vector with samples of control input values

% You could resample the data at this point if you choose to do so. %

%% Create an empty table to store the final data in.

VR_table = table('Size',[length(logs) 2], 'VariableTypes',{'double', 'double'});
VR_table.Properties.VariableNames = {'Duty_cycle', 'Aggressiveness'};

%% Calculating Duty Cycle

% Equation 3 and 4 in Niewind (2012).

for q = 1:length(logs)
    
    sumvalues = [];                                     % create variable for later use in equation % clears with every itteration/trial                                                  
    
    for i = 2:length(stick_VR{q})                       % i is number of sample % no values for sample 1
        curr_sample = stick_VR{q}(i);                   % current sample
        prev_sample = stick_VR{q}(i-1);                 % previous sample
        diff_sample = abs(curr_sample - prev_sample);   % absolute difference between samples
        if diff_sample > 0                              % threshold for change is zero % you can change this %
            sumvalues(i) = 1;                           % assign 1 if change is larger than threshold
        else   
            sumvalues(i) = 0;                           % assign zero if change is equal to or lower than threshold
        end   
    end
   
    duty_cycle{q} = sum(sumvalues)/length(stick_VR{q}); % Final calculation
    VR_table.Duty_cycle(q) = duty_cycle{q};             % Add values to Table

end

%% Calculating Aggressiveness

% Equation 6 in Niewind (2012).

for q = 1:length(logs)
       
    aggressiveness_total_diff = [];                     % create variable for later use in equation % clears with every itteration/trial                                                  
    
    for i = 2:length(stick_VR{q})                       % i is number of sample % no values for sample 1
        curr_sample = stick_VR{q}(i);                   % current sample
        prev_sample = stick_VR{q}(i-1);                 % previous sample
        diff_sample = abs(curr_sample - prev_sample);   % absolute difference between samples
        squared_diff = diff_sample^2;
        aggressiveness_total_diff(i) = squared_diff;    % stores squared difference for all samples
    end
 
    aggressiveness{q} = sqrt((1/ (length(stick_VR{q})-1)   ) * sum(aggressiveness_total_diff)); % Final calculation
    VR_table.Aggressiveness(q) = aggressiveness{q};                                              % Add values to Table
end

%% Save as .csv

writetable(VR_table, 'Table_DC_Agg.csv')    % save Table as .csv in datadir