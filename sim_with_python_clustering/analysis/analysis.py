import matplotlib.pyplot as plt
import numpy as np

def get_speeds_in_lane(src):
    """
        Takes a src string and opens a csv file,
        grabs the collumn related to speeds in lane,
        and processes it into a list of list of lists.
        Where each sublist is an iteration of the simulation
        and each sublist in that contains the speeds in a 
        given lane
    """
    file = open(src, 'r')
    lines = file.read().splitlines()[7:]
    output = []
    for line in lines[1:]:
        line = line.split(',')[14]
        number = ''
        array = []
        array_num = -1
        line = line.strip('"')
        line = list(line)[1:-1]
        for char in line:
            if char == '[':
                array.append([])
                array_num += 1
            elif char == ']':
                pass
            elif char == ' ' and number != '':
                array[array_num].append(float(number))
                number = ''
            else:
                number += char
        output.append(array)
        file.close()
    return output

def find_variance(speeds_in_lane):
    """
        Specific use case:
            Takes in a list of the speeds in each lane, 
            loops through it, turns each sublist into a 
            np array, computes the variance. This variance
            is appended to the output.
        General use case:
            return the variance of a given list. Ignoring 
            empty lists
            
        The function returns a list of the variances, where
        the index in the list reprosents the lane number. 
    """
    variances = []
    for speeds in speeds_in_lane:
        if len(speeds) > 0:
            speeds = np.array(speeds)
            variances.append(np.var(speeds))
        else:
            variances.append(None)
    return variances

def produce_variances(speeds_in_lane):
    """
        Produces a list of all the variances, from
        all of the simulation iterations.
    """
    variances = []
    for lane_speeds in speeds_in_lane:
        variances.append(find_variance(lane_speeds))

    variances_in_each_lane = [[], [], [], [], [], [], [], [], [], []] # each sublist will contain the variances for that lane index in each simulation case (ignoring None values)
    for variance in variances:
        for lane in range(len(variance)):
            if variance[lane] is not None:
                variances_in_each_lane[lane].append(variance[lane])
    return variances_in_each_lane

def produce_average_variances(speeds_in_lane):
    """
        Returns the average variance of each lane
    """
    averages = []
    variances_in_each_lane = produce_variances(speeds_in_lane)
    for variance in variances_in_each_lane:
        if len(variance) > 0:
            averages.append(np.average(variance))
        else:
            averages.append(None)
    return averages

def produce_max_min_variances(speeds_in_lane):
    """
        Returns a list, which contains tuples
        of form: (minVar, maxVar). There is a
        tuple for each lane
    """
    max_mins = []
    variances_in_each_lane = produce_variances(speeds_in_lane)
    for variances in variances_in_each_lane:
        if len(variances) > 0:
            max_mins.append((min(variances), max(variances)))
        else:
            max_mins.append(None)
    return max_mins


def main():
    """
        Perform an analysis of the swim speed distribution
    """
    is_processing = True
    output_name = input("Enter the outputfile name (don't add extension, it will be treated as a .txt). \nWarning: any file entered will be rewritten: ")
    output_doc = open(f"{output_name}.txt", 'w')
    while is_processing:
        src = input("Please enter csv file, or type stop to end: ")
        if src.upper() == 'STOP':
            is_processing = False
        else:
            speeds_in_lane = get_speeds_in_lane(src)
            output = find_variance(produce_variances(speeds_in_lane))
            output_doc.write(f"{src}\nvariance of variances\n{output}\n")
            output = produce_average_variances(speeds_in_lane)
            output_doc.write(f"\naverage of variances\n{output}\n")
            output = produce_max_min_variances(speeds_in_lane)
            output_doc.write(f"\nmax and min of variances, in form (minVar, maxVar)\n{output}\n")

    output_doc.close()

main()
