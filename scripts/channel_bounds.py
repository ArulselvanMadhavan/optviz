import torch
import torchvision
from idiom.db.client import IdiomDBClient
import csv
from channel_stats import any, all
import json

port=9004
client=IdiomDBClient(port=port)

def calc_range(pname, lname, run_id, x, w, f):
    print(f"{pname}|{lname}")
    x = x[0] # Take first sample from batch
    x_abs = torch.abs(x)
    w_abs = torch.abs(w)
    x_max = torch.max(x_abs).item()
    w_max = torch.max(w_abs).item()
    x = x / x_max
    w = w / w_max
    out = torch.matmul(x, w.T)
    f.write("{},{},{},{},{}\n".format(pname, lname, run_id, torch.min(out), torch.max(out)))
    return None


def gen_channel_range(client, projects, runs_dict={}, max_batches=1):
    with open("opt125m_range.csv", "w+") as f:
        columns = ["project", "layer", "run", "min_value", "max_value"]
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
                    for lname in layer_names:
                        if any(lname, ["attn", "attention", "ffn", "fc"]) and all(lname, ["dropout", "norm", "layernorm", "softmax"]):
                            try:
                                layer_var = client.load_layer_variables(project_name, run_id, lname)
                                weight = layer_var.get("weight", None)
                                if weight is not None:
                                    inputs = client.load_layer_inputs(project_name, run_id, lname, input_batch_id)
                                    if len(inputs) == 1:
                                        x = inputs[0][0]
                                        calc_range(project_name, lname, run_id, x, weight, f)
                            except Exception as e:
                                print("Error:", e)


gen_channel_range(client, ["opt125m"], runs_dict={"fp32":True,"dft_eval":True}, max_batches=1)
                            
        
