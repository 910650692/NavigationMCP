package com.fy.navi.service.define.aos;

import com.fy.navi.service.define.bean.PreviewParams;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RestrictedArea implements Serializable {
    private ArrayList<ArrayList<RestrictedAreaDetail>> mRestrictedAreaDetails = new ArrayList<>();
    private ArrayList<String> mCityNames = new ArrayList<>();
    private ArrayList<Integer> mCityPosition = new ArrayList<>();
    private List<PreviewParams.PointD> mPointList = new ArrayList<>();
    private long mRequestId;
}
