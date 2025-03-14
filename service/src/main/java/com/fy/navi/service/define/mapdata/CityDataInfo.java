package com.fy.navi.service.define.mapdata;

import java.util.ArrayList;

public class CityDataInfo {
    // 行政编码
    public int adcode;
    // 行政区域类型
    public int areaType;
    // 区域名称，如福建省、厦门市
    public String name;
    // 区域拼音简拼，福建省（fjs）
    public String jianPin;
    // 区域拼音全拼，福建省（fujiansheng）
    public String pinYin;
    // 上级行政区域adcode
    public int upperAdcode;
    // 同级附近5个行政区adcode列表
    public ArrayList<Integer> vecNearAdcodeList;
    // 下级行政区Id列表
    public ArrayList<Integer> vecLowerAdcodeList;
    // 城市数据包下载状态信息
    public CityDownLoadInfo downLoadInfo;
    // 城市经度
    public double cityX;
    // 城市纬度
    public double cityY;
}
