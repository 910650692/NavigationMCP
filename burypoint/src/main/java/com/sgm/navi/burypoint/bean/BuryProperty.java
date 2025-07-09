package com.sgm.navi.burypoint.bean;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.List;

public class BuryProperty {

    private List<BuryParam> params;

    public List<BuryParam> getParams() {
        return params;
    }

    public void setParams(List<BuryParam> params) {
        this.params = params;
    }

    public BuryProperty(){

    }

    public BuryProperty(String key, String value){
        params.add(new BuryParam(key,value));
    }

    public BuryProperty(List<BuryParam> params){
        this.params = params;
    }

    public BuryProperty(Builder builder){
        this.params = builder.params;
    }

    public static class Builder{
        private List<BuryParam> params = new ArrayList<>();

        public Builder setParams(String key, String value) {
            params.add(new BuryParam(key,value));
            return this;
        }

        public Builder setParams(List<BuryParam> params) {
            this.params = params;
            return this;
        }

        public BuryProperty build(){
            return new BuryProperty(this);
        }
    }

    @NonNull
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("BuryProperty{");
        if (params != null && !params.isEmpty()) {
            for (BuryParam param : params) {
                sb.append(param.getKey()).append("=").append(param.getValue()).append(", ");
            }
            sb.setLength(sb.length() - 2); // Remove the last comma and space
        }
        sb.append('}');
        return sb.toString();
    }
}
