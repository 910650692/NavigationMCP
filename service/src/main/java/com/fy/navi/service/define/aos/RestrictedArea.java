package com.fy.navi.service.define.aos;

import java.io.Serializable;
import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RestrictedArea implements Serializable {
    public ArrayList<ArrayList<RestrictedAreaDetail>> restrictedAreaDetails = new ArrayList<>();
    public ArrayList<String> cityNames = new ArrayList<>();
    public ArrayList<Integer> cityPosition = new ArrayList<>();
    private long requestId;
    public RestrictedArea() {
    }
}
