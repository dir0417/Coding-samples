#! /usr/bin/env python

#### DESCRIPTION #####
# adds horizon metdata to a csv file with accession ids

# usage
# add_horizon_metadata.py -i <results.csv> -o <output_directory> --pui <horizon_cov_pui_dataset> --varseq <horizon_var_seq_dataset>

import argparse
import sys
import pandas as pd
import numpy as np
from datetime import date
import re
import os
# zip code stuff
from uszipcode import SearchEngine, SimpleZipcode, Zipcode



#### FUNCTIONS #####
def getOptions(args=sys.argv[1:]):
    parser = argparse.ArgumentParser(description="Parses command.")
    parser.add_argument("-i", "--input", help="results csv file with a column for the sequence accession id called 'accession_id'")
    parser.add_argument("-o", "--output", help="specifies the output directory where the output results where be written to; if not specified, the output results will be written to the current directory")
    parser.add_argument("--pui",  help="csv file of the COV_PUI horizon dataset query")
    parser.add_argument("--varseq", help="csv file of the CV_VAR_SEQ horizon dataset query")
    options = parser.parse_args(args)
    return options


def merge_horizon_datasets(pui_path, varseq_path):
    print('merging COV_PUI dataset query and CV_VAR_SEQ dataset query')

    cv_var_seq = pd.read_csv(varseq_path, dtype = {'hsn': object, 'HSN' : object})


    col_rename = {'HSN' : 'accession_id',
                  'hsn' : 'accession_id',
                  'collect_date' : 'collection_date',
                  'cust_name' : 'cust_name',
                 'Collect Date' : 'collection_date',
                 'Receive Date' : 'receive_date',
                 'First Name' : 'first_name',
                 'Last Name' : 'last_name',
                 'County': 'county',
                 'ZIP' : 'zip',
                 'Cust Name' : 'cust_name',
                 'DOB' : 'dob'}

    col_order = ['accession_id', 'collection_date', 'cust_name',
                 'first_name', 'last_name',  'dob', 'zip', 'county', 'receive_date']

    
    cv_var_seq = cv_var_seq.rename(columns = col_rename)
    cv_var_seq = cv_var_seq.drop_duplicates(subset = 'accession_id', keep = 'first')
    cv_var_seq = cv_var_seq[col_order]



    covpui = pd.read_csv(pui_path, dtype = {'cdphe_lab_id' : object})
    covpui.columns


    drop_col = ['batch_number', 'comp_date', 'customer_sample_id', 'cust_id', 'flags', 'proc_code', 'cmp_text']

    col_rename = {'HSN' : 'accession_id',
                  'cdphe_lab_id' : 'accession_id',
                    'hsn' : 'accession_id',
                  'collect_date' : 'collection_date',
                  'cust_name' : 'cust_name',
                 'Customer' : 'cust_name',
                  'customer' : 'cust_name',
                  'Last Name' : 'last_name',
                  'First Name' : 'first_name',
                  'County' : 'county',
                  'P Zip' : 'zip',
                  'p_zip' : 'zip',
                  'date_of_birth' : 'dob',
                  'customer' : 'cust_name',
                  'Receive Date': 'receive_date',
                  'Date Of Birth' : 'dob',
                  'Collect Date' : 'collection_date'}



    col_order = ['accession_id', 'collection_date', 'cust_name',
                 'first_name', 'last_name', 'dob',  'zip', 'county', 'receive_date']
    
    covpui = covpui.drop(columns = drop_col)
    covpui = covpui.rename(columns = col_rename)
    covpui = covpui.drop_duplicates(subset = 'accession_id', keep = 'first')
    covpui = covpui[col_order]


    dataframe_list = []
    dataframe_list.append(cv_var_seq)
    dataframe_list.append(covpui)
    df = pd.concat(dataframe_list)
    df = df.drop_duplicates(subset = 'accession_id', keep = 'first')
    df = df.reset_index(drop = True)

    return df

def filter_query(query, results_csv_path):
    print('filtering query to accession_ids included in results csv file')
    results = pd.read_csv(results_csv_path, dtype = {'accession_id' : object}, sep = '\t')
    
    def remove_dashes (accession_id):
        if re.search('\d{10}-\d', accession_id):
            return re.findall('(\d{10})-\d'. accession_id)[0]
        else:
            return accession_id
        
#     to_use_accession_id = results.apply(lambda x:remove_dashes(x.accession_id), axis=1)
#     results.insert(loc=1, column='accession_for_joining', value= to_use_accession_id)

    accession_ids = results.accession_id.unique().tolist()

    crit = query.accession_id.isin(accession_ids)
    f_query = query[crit]
    f_query = f_query.reset_index(drop = True)

    return f_query

def clean_merged(query):
    print('cleaning filtered merged dataset query')
    print('....there are %s lines of data in query' % query.shape[0])

    ##################################
    # fix dates
#     print('....fixing date formats')
#     # 2 - fix the collect date
#     def fix_date_string(val):
#         x = re.sub(r'\s\d{2}:\d{2}:\d{2}\s\w{3}-\d+\s', '', str(val))
#         y = re.sub(r'\([\\a-zA-Z\s]+\)', '', str(x) )
#         return y


    query.collection_date = pd.to_datetime(query.collection_date, errors='coerce').dt.date
    query.dob = pd.to_datetime(query.dob, errors='coerce')
    query.receive_date = pd.to_datetime(query.receive_date, errors='coerce').dt.date
    
    #query.collection_date = pd.to_datetime(query.collection_date)
#     query.collect_date = query.collect_date.fillna(value = 'no date')
    
     
    ##################################
    # clean up patient
    print('  ....cleaning up patient and customer names')

    query.last_name = query.last_name.str.strip()
    query.last_name = query.last_name.str.capitalize()

    query.first_name = query.first_name.str.strip()
    query.first_name = query.first_name.str.capitalize()


    ##########################################
    # clean up customer and get customer county
    query.cust_name = query.cust_name.str.strip()
    def remove_character(customer):
        if re.search('\'', customer):
            return customer.replace('\'', '')
        else:
            return customer
    query.cust_name = query.apply(lambda x:remove_character(x.cust_name), axis = 1)

    correct_customer_dict = {
       name of customers
    }

    def correct_customer_names(customer):
        if customer in correct_customer_dict.keys():
            return correct_customer_dict[customer]
        else:
            return customer

    query.cust_name = query.apply(lambda x:correct_customer_names(x.cust_name), axis = 1)

    # get the patient county and state data
    print('....getting patient county and state')

    # load in the state abrrevation dictionary
    state_dict = {List of US states}

    query.zip = query.zip.fillna('not provided')
    query.county = query.county.fillna('not provided')

    # get ride of wrong or na zip codes
    for i in range(query.shape[0]):
        query.at[i, 'zip'] = str(query.zip[i])
        if re.search('\d{5}', query.zip[i]):
            query.at[i, 'zip'] = re.findall('\d+', query.zip[i])[0]
        else:
            query.at[i, 'zip'] = 'not provided'

    def get_county(p_zip):
        if p_zip != 'not provided':
#             print(p_zip)
            p_zip = p_zip[0:5]
            p_zip = int(p_zip)
            search = SearchEngine()
            zipcode = search.by_zipcode(p_zip)
            if zipcode.county != None:
                county_name = zipcode.county.replace('County', '').strip().upper()
                return county_name
            else:
                return ''
        else:
            return ''

    def get_state(p_zip, state_dict):
        if p_zip != 'not provided'  and re.search('\d+', p_zip):
            p_zip = re.findall('\d+', p_zip)[0]
            search = SearchEngine()
            zipcode = search.by_zipcode(p_zip)
            if zipcode.state in state_dict.keys():
                state = state_dict[zipcode.state]
                return state
        elif p_zip == 'not provided':
            return ''


    p_county = query.apply(lambda x:get_county(x.zip), axis=1)

    query.insert(loc=0, column='p_county', value=p_county)

    p_state = query.apply(lambda x:get_state(x.zip, state_dict), axis=1)
    query.insert(loc=0, column='p_state', value = p_state)

    # try to fill in any missing county data with query.county
    for i in range(query.shape[0]):
        if query.p_county[i] is np.NaN and query.county[i] != 'not provided' :
            query.at[i, 'p_county'] = query.county[i]



    ##################################


    print('....finished cleaning query')
    return query

def merge_results_and_query(cleaned_query, results_path):
    print('adding metadata to results file')
    cleaned_query = cleaned_query.set_index('accession_id')

    # set results index for join
    results = pd.read_csv(results_path, dtype = {'accession_id' : object}, sep = '\t')
    results = results.set_index('accession_id')

    j = results.join(cleaned_query, how = 'left')
    j = j.reset_index()
    j = j.rename(columns = {'collection_date' : 'date'})
    
    print('....correcting collection date')
    collection_date_dict = {collection date list}
    
    for row in range(j.shape[0]):
        accession_id = j.accession_id[row]
        if accession_id in collection_date_dict.keys():
            j.at[row, 'collection_date'] = collection_date_dict[accession_id]
    
    return j


if __name__ == '__main__':

    print('')
    print('ADDING HORIZON METADATA TO RESULTS CSV')
    print('')

    options = getOptions()

    # check agrument inputs
    if options.input is None or not os.path.exists(options.input):
        raise Exception('no csv file with "accession_id" as field is specified or does not exist')
    else:
        results = pd.read_csv(options.input, sep = '\t')
        if 'accession_id' not in results.columns:
            raise Exception("results csv file does not contain a column called 'accession_id' with accession ids")

    if options.output is None:
        outdirectory == os.getcwd()
    else:
        outdirectory = options.output

    if options.pui is None or not os.path.exists(options.pui):
        raise Exception('COV_PUI dataset path is not specified or does not exist')

    if options.varseq is None or not os.path.exists(options.varseq):
        raise Exception('CV_VAR_SEQ dataset path is not specified or does not exist')



    ## run functions as workflow ##

    merged_queries = merge_horizon_datasets(pui_path =options.pui,
                                      varseq_path = options.varseq)

    filtered_query = filter_query(query = merged_queries,
                                  results_csv_path = options.input)

    cleaned_query = clean_merged(query = filtered_query)

    final_df = merge_results_and_query(cleaned_query = cleaned_query,
                                       results_path = options.input)

    # save final_df
    # grab input file name 
    file_input_name = re.findall('([0-9a-zA-Z_\-]+).tsv', options.input)[0]
    outfile = os.path.join(outdirectory, '%s_metadata-internal.tsv' % (file_input_name))
    print('writing output to %s' % outfile)
    final_df.to_csv(outfile, index=False, sep = '\t')

    print('Done!')
