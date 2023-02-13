import torch
import torchvision
from idiom.db.client import IdiomDBClient
from channel_stats import any, all
import numpy as np
import json
import os

port=9004
client=IdiomDBClient(port=port)
TOP_K=1000
TOP_SELECT=1000
SAMPLE_LEN=5000

def compute_bounds(x):
    q = torch.tensor([0.25, 0.5, 0.75])
    q_out = torch.quantile(x, q)
    q1, median, q3 = q[0], q[1], q[2]
    iqr = q3 - q1
    k = 1.5
    lower_bound = q1 - (k * iqr)
    upper_bound = q3 + (k * iqr)
    return lower_bound, upper_bound, q1, q3, median
    
def percent_outliers(client, projects, runs_dict={"fp32":True}, max_batches=1):
    with open("data/opt_outlier_counts.csv", "w+") as f:
        columns = ["project", "layer", "ol_lower", "ol_higher", "ol_percent"]
        f.write(",".join(columns))
        f.write("\n")
        for project_name in projects:
            proj_info = client.get_project(project_name)
            runs = proj_info["project"]["runs"]
            runs = [r for r in runs if runs_dict.get(r["name"], False)]
            for run in runs:
                run_id = run["name"]
                input_batch_ids = client.get_input_batches(project_name, run_id)
                input_batch_ids = list(input_batch_ids.keys())[:max_batches]
                run_info = client.get_run(project_name, run_id)
                layer_names = [l["name"] for l in run_info["run"]["layers"]]

                for input_batch_id in input_batch_ids:
                    print("batch id:", project_name, run, input_batch_id)
                    client.current_input_batch_id = input_batch_id
                    total_rows = 0
                    for lname in layer_names:
                        if any(lname, ["attn", "attention", "ffn", "fc"]) and all(lname, ["dropout", "norm", "layernorm", "softmax"]):
                            try:
                                layer_var = client.load_layer_variables(project_name, run_id, lname)
                                weight = layer_var.get("weight", None)
                                if weight is not None:
                                    inputs = client.load_layer_inputs(project_name, run_id, lname, input_batch_id)
                                    if len(inputs) == 1:
                                        x = inputs[0][0]
                                        if len(x.shape) == 2:
                                            x = x.reshape(8, x.shape[0]//8, x.shape[1])                                    
                                        if len(x.shape) == 3:
                                            x = x[0]
                                        x = x.flatten()                                            
                                        lower_bound, upper_bound, _, _, _ = compute_bounds(x)
                                        q1_outliers = torch.where(x < lower_bound, x, 0)
                                        q1_outliers = torch.nonzero(q1_outliers)
                                        q1_ol_count = np.prod(q1_outliers.shape)
                                        q3_outliers = torch.where(x > upper_bound, x, 0)
                                        q3_outliers = torch.nonzero(q3_outliers)
                                        q3_ol_count = np.prod(q3_outliers.shape)
                                        total_count = np.prod(x.shape)
                                        f.write("{},{},{},{},{}\n".format(project_name, lname, q1_ol_count, q3_ol_count, (q1_ol_count + q3_ol_count) / total_count))
                                        #print(q1_ol_count, q3_ol_count, (q1_ol_count + q3_ol_count) / total_count)
                            except Exception as e:
                                print("Error:", e)


def check_and_create_dir(path):
    if not os.path.exists(path):
        os.makedirs(path)
    return None

def boxplot_data(client, projects, runs_dict={"fp32":True}, max_batches=1):
    for project_name in projects:
        fdir = f"data/{project_name}"
        check_and_create_dir(fdir)
        fname = fdir + "/opt_boxplot"
        with open(fname + "_stats.csv", "w+") as sf:
            columns = ["layer_id","layer","lower","upper","q1","q3","median", "total", "q1_count", "q3_count", "min_outlier", "max_outlier"]
            sf.write(",".join(columns))
            sf.write("\n")
            with open(fname + "_outliers.csv", "w+") as of:
                columns = ["layer_id", "outliers"]
                of.write(",".join(columns))
                of.write("\n")
            # f.write("[\n")
            # is_first = True
                proj_info = client.get_project(project_name)
                runs = proj_info["project"]["runs"]
                runs = [r for r in runs if runs_dict.get(r["name"], False)]
                for run in runs:
                    run_id = run["name"]
                    input_batch_ids = client.get_input_batches(project_name, run_id)
                    input_batch_ids = list(input_batch_ids.keys())[:max_batches]
                    run_info = client.get_run(project_name, run_id)
                    layer_names = [l["name"] for l in run_info["run"]["layers"]]

                    for input_batch_id in input_batch_ids:
                        print("batch id:", project_name, run, input_batch_id)
                        client.current_input_batch_id = input_batch_id
                        total_rows = 0
                        layer_idx = 0
                        for lname in layer_names:
                            if any(lname, ["attn", "attention", "ffn", "fc"]) and all(lname, ["dropout", "norm", "layernorm", "softmax"]):
                                try:
                                    layer_var = client.load_layer_variables(project_name, run_id, lname)
                                    weight = layer_var.get("weight", None)
                                    if weight is not None:
                                        inputs = client.load_layer_inputs(project_name, run_id, lname, input_batch_id)
                                        if len(inputs) == 1:
                                            # if not is_first:
                                            #     f.write(",")
                                            x = inputs[0][0]
                                            if len(x.shape) == 2:
                                                x = x.reshape(8, x.shape[0]//8, x.shape[1])                                    
                                            if len(x.shape) == 3:
                                                x = x[0]
                                            x = x.flatten()                                        
                                            lb, ub, q1, q3, m = compute_bounds(x)
                                            q1 = q1.item()
                                            q3 = q3.item()
                                            m = m.item()
                                            lb = lb.item()
                                            ub = ub.item()
                                            q1_quartile, q3_quartile = x[x < lb], x[x > ub]
                                            q1_count, q3_count = np.prod(q1_quartile.shape), np.prod(q3_quartile.shape)
                                            total = q1_count + q3_count
                                            q1_top, q3_top = torch.topk(x, TOP_K, largest=False).values.tolist(), torch.topk(x, TOP_K, largest=True).values.tolist()
                                            q1_edge, q3_edge = q1_top[TOP_K - 1], q3_top[TOP_K - 1]

                                            out = torch.where((x < lb) & (x > q1_edge), x, 0)
                                            q1_mult = out.unique()
                                            out = torch.where((x > ub) & (x < q3_edge), x, 0)
                                            q3_mult = out.unique()
                                            #print(q1_count, q1_mult.shape)
                                            q1_m_shape, q3_m_shape = np.prod(q1_mult.shape), np.prod(q3_mult.shape)
                                            if total == 0:
                                                q1_percent, q3_percent = 0, 0
                                            else:
                                                q1_percent, q3_percent = q1_count / total, q3_count / total
                                            q1_slen, q3_slen = int(SAMPLE_LEN * q1_percent), int(SAMPLE_LEN * q3_percent)
                                            q1_rand, q3_rand = q1_mult[torch.randperm(q1_m_shape)[:q1_slen]], q3_mult[torch.randperm(q3_m_shape)[:q3_slen]]
                                            q1_rand, q3_rand = q1_rand.tolist(), q3_rand.tolist()
                                            outliers = q1_top[:TOP_SELECT] + q1_rand + q3_top[:TOP_SELECT] + q3_rand
                                            #print(q1_count, len(q1_top) + len(q1_rand), q3_count, len(q3_top) + len(q3_rand))
                                            #row = {"layer":lname, "project":project_name, "lower":lb, "upper":ub, "q1":q1, "q3":q3, "median":m, "outliers":outliers, "total": int(total), "q1_count":int(q1_count), "q3_count":int(q3_count)}
                                            #columns = ["layer_id","layer","lower","higher","q1","q3","median", "total", "q1_count", "q3_count"]
                                            sf.write("{},{},{},{},{},{},{},{},{},{},{},{}\n".format(layer_idx, lname, lb, ub, q1, q3, m, total, q1_count, q3_count, q1_top[0], q3_top[0]))
                                            for ol in outliers:
                                                of.write("{},{}\n".format(layer_idx, ol))
                                            #json.dump(row, f)
                                            #f.write("\n")
                                            #is_first = False
                                except Exception as e:
                                    print(q1_count, total, q3_count)
                                    print("Error:", e)
                            layer_idx += 1

        

#percent_outliers(client, ["opt125m", "opt350m", "opt1.3b", "opt2.7b"], runs_dict={"fp32":True}, max_batches=1)            
boxplot_data(client, ["opt125m", "opt350m", "opt1.3b", "opt2.7b"], runs_dict={"fp32":True}, max_batches=1)
