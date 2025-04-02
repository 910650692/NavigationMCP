package com.fy.navi.burypoint.bean;

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

}
