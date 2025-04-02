package com.fy.navi.service.define.route;

import com.fy.navi.service.define.map.MapType;

import java.util.ArrayList;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteTMCParam {
    /*** 请求Id **/
    private long mRequestId;
    /*** 屏幕Id **/
    private MapType mMapTypeId;
    /*** 家或公司 0：家， 1：公司 **/
    private int mKey;
    /*** 是否距离近 **/
    private boolean mIsShort;
    /*** 是否距离近 **/
    private String mTime = "计算中...";
    /*** TMC信息 **/
    private List<RouteLightBarItem> mRouteLightBarItem = new ArrayList<>() ;
}
