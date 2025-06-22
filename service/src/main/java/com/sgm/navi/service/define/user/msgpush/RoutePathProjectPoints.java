package com.sgm.navi.service.define.user.msgpush;


import com.autonavi.gbl.aosclient.model.RouteViaProjInfo;

import java.util.ArrayList;

public class RoutePathProjectPoints {
    // path的索引
    public int path_idx;
    // 所有途经点到idx为0的path上投影点信息
    public ArrayList<RouteViaProjInfo> via_proj_info;

    public int id_mode;
    public ArrayList<String> id;

    // 名称: idx 含义: 编号, 0 是显示点，投影点按照顺序idx递增
    public int idx;
    // 名称: type 含义: 类型 0：GPS坐标；1：手动选点；2：POI坐标
    public int type;
    // 名称: lon 含义: 经度
    public String lon;
    // 名称: lat 含义: 纬度
    public String lat;

    public int getPath_idx() {
        return path_idx;
    }

    public void setPath_idx(int path_idx) {
        this.path_idx = path_idx;
    }

    public ArrayList<RouteViaProjInfo> getVia_proj_info() {
        return via_proj_info;
    }

    public void setVia_proj_info(ArrayList<RouteViaProjInfo> via_proj_info) {
        this.via_proj_info = via_proj_info;
    }

    public int getId_mode() {
        return id_mode;
    }

    public void setId_mode(int id_mode) {
        this.id_mode = id_mode;
    }

    public ArrayList<String> getId() {
        return id;
    }

    public void setId(ArrayList<String> id) {
        this.id = id;
    }

    public int getIdx() {
        return idx;
    }

    public void setIdx(int idx) {
        this.idx = idx;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public String getLon() {
        return lon;
    }

    public void setLon(String lon) {
        this.lon = lon;
    }

    public String getLat() {
        return lat;
    }

    public void setLat(String lat) {
        this.lat = lat;
    }
}
