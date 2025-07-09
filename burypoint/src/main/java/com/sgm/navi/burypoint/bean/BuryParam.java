package com.sgm.navi.burypoint.bean;

import androidx.annotation.NonNull;

public class BuryParam {
    private String key;
    private String value;

    public BuryParam(){
    }

    public BuryParam(String key, String value){
        this.key = key;
        this.value = value;
    }

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    @NonNull
    @Override
    public String toString() {
        return "BuryParam{" +
                "key='" + key + '\'' +
                ", value='" + value + '\'' +
                '}';
    }
}
