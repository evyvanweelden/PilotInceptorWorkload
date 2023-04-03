%% %%%% ANALYZING FLIGHT LOGS %%%% %%

% By Evy van Weelden and Carl van Beek, Tilburg University
% Last updated on 03-04-2023
% FOrmulas are based on equations from:	I. Niewind, “Pilot Gain and the Workload Buildup Flight Test Technique: A Closer Investigation of Pilot Inceptor Workload”, DLR-Interner Bericht, Report No. IB 111-2012/74, Oct. 2012, URL: https://elib.dlr.de/88208/. 

%% BASIC INFO of Matlab:
% Percent sign means row is commented/will be skipped when running code. Delete "%" from rows to use commands.
% Run full code by pressing "Run" in tab above (large green triangle).
% Select(with mouse) + press F9(on keyboard) runs the selected commands only.
% Equal sign ("=") assigns data to variable name, example: variable1 = data.
% Data is stored in matrices. matrix1 = [data];
% Cell structures can hold matrices. cell1 = {data}; / cell1 = {matrix1}

%% INFO SCRIPT
% This script uses .h5 files and functions 'dsim_read_variable_from_log.m' and 'dsim_resample_data.m'.

%% get ready

clc                                                     % empties command window
clear                                                   % removes variables from workspace

matdir = '';                                            % path .mat scripts  % edit
addpath(genpath(matdir));

datadir = '';                                           % path data files    % edit
cd(datadir)                                             % change working directory to path with data files

%% Resampling log data and calculating Duty Cycle and Aggressiveness

% Setting variables which are used to create a table.

VR_table = table('Size',[72 9], 'VariableTypes',{'cell','cell','double', 'double', 'double', 'double', 'double', 'double', 'string'});
VR_table.Properties.VariableNames = {'Longitudinal_data', 'Lateral_data', 'Duty_cycle_longitudinal', 'Aggressiveness_longitudinal', 'Duty_cycle_lateral', 'Aggressiveness_lateral', 'Subject', 'Trial', 'Condition'};
stick_VR_longitudinal = {};     % for longitudinal stick data
stick_VR_lateral = {};          % for lateral stick data
logs = dir('*.h5');

% Loading longitudinal data and resampling (to 100 Hz) 
% & Calculating Duty Cycle and Aggressiveness

for q = 1:length(logs)
    duty_cycle_longitudinal = {};
    aggressiveness_longitudinal = {};

    [phi, time_start, time_end] = dsim_read_variable_from_log((logs(q).name),'control/pilot/stick/longitudinal'); % read file
    time_start = double(time_start);
    time_end = double(time_end);
    duration = ((time_end-time_start)/1e6);                             % duration in seconds
 
    time = 0:0.01:duration;                                             % 1 sample per 0.01 sec - 100 Hz

    
    resampled_phi = dsim_resample_data(time, phi, double(time_start));  % Resample
    stick_VR_longitudinal{q} = resampled_phi;                           % Store data for file no. q in cell
    subjectname = logs(q).name;
    VR_table.Longitudinal_data(q) = stick_VR_longitudinal(q);           % Transfer resampled data of file no. q to Table
    VR_table.Properties.RowNames{q} = subjectname;                      % Subject pseudonym as rowname
    
    % Duty Cycle
    % Equation 3 and 4 in Niewind (2012).
    somwaarden = [];                                                    
    for i = 2:length(stick_VR_longitudinal{q})                          % i is number of sample % no values for sample 1
        curr_sample = stick_VR_longitudinal{q}(i);
        prev_sample = stick_VR_longitudinal{q}(i-1); 
        diff_sample = abs(curr_sample - prev_sample);
        if diff_sample > 0                                              % threshold for change is zero % you can change this
            somwaarden(i) = 1;                                          % 1 if change is larger than threshold
        else   
            somwaarden(i) = 0;                                          % zero if change is equal to or lower than threshold
        end   
    end
    duty_cycle_longitudinal{q} = sum(somwaarden)/length(stick_VR_longitudinal{q}); 
    VR_table.Duty_cycle_longitudinal(q) = duty_cycle_longitudinal{q};   % Add to Table
    
    % Aggressiveness
    % Equation 6 in Niewind (2012).
    aggressiveness_total_diff = [];
    for i = 2:length(stick_VR_longitudinal{q})                          % i is number of sample % no values for sample 1
        curr_sample = stick_VR_longitudinal{q}(i);
        prev_sample = stick_VR_longitudinal{q}(i-1); 
        diff_sample = abs(curr_sample - prev_sample);
        squared_diff = diff_sample^2;
        aggressiveness_total_diff(i) = squared_diff;
    end
 
    aggressiveness_longitudinal{q} = sqrt((1/ (length (stick_VR_longitudinal{q})-1)   ) * sum(aggressiveness_total_diff)); % Calculation
    VR_table.Aggressiveness_longitudinal(q) = aggressiveness_longitudinal{q};                                              % Add to Table
end

% Loading lateral data and resampling (to 100 Hz)
% & Calculating Duty Cycle and Aggressiveness

for r = 1:length(logs)
    duty_cycle_lateral = {};
    aggressiveness_lateral = {};
    [phi, time_start, time_end] = dsim_read_variable_from_log((logs(r).name),'control/pilot/stick/lateral'); % read file
    time_start = double(time_start);
    time_end = double(time_end);
    duration = ((time_end-time_start)/1e6);                             % duration in seconds
 
    time = 0:0.01:duration;                                             % 1 sample per 0.01 sec - 100 Hz

    resampled_phi = dsim_resample_data(time, phi, double(time_start));  % Resample
    stick_VR_lateral{r} = resampled_phi;                                % Store data for file no. q in cell
    subjectname = logs(r).name;
    VR_table.Lateral_data(r) = stick_VR_lateral(r);                     % Transfer resampled data of file no. q to Table
    VR_table.Properties.RowNames{r} = subjectname;                      % Subject pseudonym as rowname
    
    % Duty Cycle
    % Equation 3 and 4 in Niewind (2012).  
    somwaarden = [];
    for i = 2:length(stick_VR_lateral{r})                               % i is number of sample % no values for sample 1
        curr_sample = stick_VR_lateral{r}(i);
        prev_sample = stick_VR_lateral{r}(i-1); 
        diff_sample = abs(curr_sample - prev_sample);
        if diff_sample > 0                                              % threshold for change is zero % you can change this
            somwaarden(i) = 1;                                          % 1 if change is larger than threshold
        else   
            somwaarden(i) = 0;                                          % zero if change is equal to or lower than threshold
        end   
    end
    duty_cycle_lateral{r} = sum(somwaarden)/length(stick_VR_lateral{r});
    VR_table.Duty_cycle_lateral(r) = duty_cycle_lateral{r};             % Add to Table

    % Aggressiveness
    % Equation 6 in Niewind (2012).
    aggressiveness_total_diff = [];
    for i = 2:length(stick_VR_lateral{r})                               % i is number of sample % no values for sample 1
        curr_sample = stick_VR_lateral{r}(i);
        prev_sample = stick_VR_lateral{r}(i-1); 
        diff_sample = abs(curr_sample - prev_sample);
        squared_diff = diff_sample^2;
        aggressiveness_total_diff(i) = squared_diff;
    end
    aggressiveness_lateral{r} = sqrt((1/ (length (stick_VR_lateral{r})-1)   ) * sum(aggressiveness_total_diff));   % Calculation
    VR_table.Aggressiveness_lateral(r) = aggressiveness_lateral{r};                                                % Add to Table
end

% Adding subject and trial numbers as columns to Table

for q = 1:height(VR_table)
    if startsWith(VR_table.Properties.RowNames(q),'subject1')  
        VR_table.Subject(q) = 1;
    elseif startsWith(VR_table.Properties.RowNames(q),'subject2')  
        VR_table.Subject(q) = 2;
    elseif startsWith(VR_table.Properties.RowNames(q),'subject3')  
        VR_table.Subject(q) = 3;
    elseif startsWith(VR_table.Properties.RowNames(q),'subject4')  
        VR_table.Subject(q) = 4;
    elseif startsWith(VR_table.Properties.RowNames(q),'subject5')
        VR_table.Subject(q) = 5;
    elseif startsWith(VR_table.Properties.RowNames(q),'subject6')  
        VR_table.Subject(q) = 6;
    else
        VR_table.Subject(q) = 11111;
    end
    if endsWith(VR_table.Properties.RowNames(q), 'trial1.h5')
        VR_table.Trial(q) = 1;
    elseif endsWith(VR_table.Properties.RowNames(q), 'trial2.h5')
        VR_table.Trial(q) = 2;
    elseif endsWith(VR_table.Properties.RowNames(q), 'trial3.h5')
        VR_table.Trial(q) = 3;
    elseif endsWith(VR_table.Properties.RowNames(q), 'trial4.h5')
        VR_table.Trial(q) = 4;
    elseif endsWith(VR_table.Properties.RowNames(q), 'trial5.h5')
        VR_table.Trial(q) = 5;
    elseif endsWith(VR_table.Properties.RowNames(q), 'trial6.h5')
        VR_table.Trial(q) = 6;
    elseif endsWith(VR_table.Properties.RowNames(q), 'trial7.h5')
        VR_table.Trial(q) = 7;
    elseif endsWith(VR_table.Properties.RowNames(q), 'trial8.h5')
        VR_table.Trial(q) = 8;
    elseif endsWith(VR_table.Properties.RowNames(q), 'trial9.h5')
        VR_table.Trial(q) = 9;
    elseif endsWith(VR_table.Properties.RowNames(q), 'trial10.h5')
        VR_table.Trial(q) = 10;
    elseif endsWith(VR_table.Properties.RowNames(q), 'trial11.h5')
        VR_table.Trial(q) = 11;
    elseif endsWith(VR_table.Properties.RowNames(q), 'trial12.h5')
        VR_table.Trial(q) = 12;
    else
        VR_table.Trial(q) = 11111;
    end
end

% Adding a column for conditions % Taken from study protocol and experimenter sheets.

order_of_conditions = [2;1;1;2;1;1;2;2;1;1;2;2;1;1;1;2;1;2;2;2;1;2;1;2;2;2;1;2;1;1;2;1;2;1;2;1;1;1;2;2;1;2;2;2;1;1;2;1;1;2;1;2;2;1;2;1;2;2;1;1;2;2;2;1;2;1;1;2;2;1;1;1];
for i = 1:length(order_of_conditions)
    if order_of_conditions(i) == 1
         VR_table.Condition(i) = "Low workload";
    elseif order_of_conditions(i) == 2
        VR_table.Condition(i) = "High workload";
    else
        VR_table.Condition(i) = "Error";
    end
end

writetable(VR_table, 'Table_DC_Agg.csv')    % save Table as .csv in datadir