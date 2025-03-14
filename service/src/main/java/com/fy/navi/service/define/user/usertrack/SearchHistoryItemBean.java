package com.fy.navi.service.define.user.usertrack;

import java.util.ArrayList;

/**
 * @Description
 * @Author fh
 * @date 2024/12/27
 */
public class SearchHistoryItemBean {

    public String name; // 记录搜索POI名字或者搜索词
    public String poiid; // POI唯一ID
    public String id;
    public int type;
    public int datatype;
    public double x;
    public double y;
    public double x_entr;
    public double y_entr;
    public long update_time; // 数据更新时间，单位秒
    public int history_type;
    public int iconinfo;
    public String adcode;
    public String district;
    public String address;
    public String poi_tag;
    public String func_text;
    public String short_name;
    public String display_info;
    public String search_query;
    public String terminals;
    public String ignore_district;
    public ArrayList<String> search_tag;
    public ArrayList<String> search_query_set;
    public String rich_rating;
    public String num_review;
    public String category;
    public String super_address;
    public String datatype_spec; // 记录对应记录类型，具体Icon资源需要上层关联
    public String poi;
    public String citycode;
    public String version;
    public String parent;
    public int childType;
    public String towardsAngle;
    public String floorNo;
    public int endPoiExtension;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getPoiid() {
        return poiid;
    }

    public void setPoiid(String poiid) {
        this.poiid = poiid;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public int getDatatype() {
        return datatype;
    }

    public void setDatatype(int datatype) {
        this.datatype = datatype;
    }

    public double getX() {
        return x;
    }

    public void setX(double x) {
        this.x = x;
    }

    public double getY() {
        return y;
    }

    public void setY(double y) {
        this.y = y;
    }

    public double getX_entr() {
        return x_entr;
    }

    public void setX_entr(double x_entr) {
        this.x_entr = x_entr;
    }

    public double getY_entr() {
        return y_entr;
    }

    public void setY_entr(double y_entr) {
        this.y_entr = y_entr;
    }

    public long getUpdate_time() {
        return update_time;
    }

    public void setUpdate_time(long update_time) {
        this.update_time = update_time;
    }

    public int getHistory_type() {
        return history_type;
    }

    public void setHistory_type(int history_type) {
        this.history_type = history_type;
    }

    public int getIconinfo() {
        return iconinfo;
    }

    public void setIconinfo(int iconinfo) {
        this.iconinfo = iconinfo;
    }

    public String getAdcode() {
        return adcode;
    }

    public void setAdcode(String adcode) {
        this.adcode = adcode;
    }

    public String getDistrict() {
        return district;
    }

    public void setDistrict(String district) {
        this.district = district;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public String getPoi_tag() {
        return poi_tag;
    }

    public void setPoi_tag(String poi_tag) {
        this.poi_tag = poi_tag;
    }

    public String getFunc_text() {
        return func_text;
    }

    public void setFunc_text(String func_text) {
        this.func_text = func_text;
    }

    public String getShort_name() {
        return short_name;
    }

    public void setShort_name(String short_name) {
        this.short_name = short_name;
    }

    public String getDisplay_info() {
        return display_info;
    }

    public void setDisplay_info(String display_info) {
        this.display_info = display_info;
    }

    public String getSearch_query() {
        return search_query;
    }

    public void setSearch_query(String search_query) {
        this.search_query = search_query;
    }

    public String getTerminals() {
        return terminals;
    }

    public void setTerminals(String terminals) {
        this.terminals = terminals;
    }

    public String getIgnore_district() {
        return ignore_district;
    }

    public void setIgnore_district(String ignore_district) {
        this.ignore_district = ignore_district;
    }

    public ArrayList<String> getSearch_tag() {
        return search_tag;
    }

    public void setSearch_tag(ArrayList<String> search_tag) {
        this.search_tag = search_tag;
    }

    public ArrayList<String> getSearch_query_set() {
        return search_query_set;
    }

    public void setSearch_query_set(ArrayList<String> search_query_set) {
        this.search_query_set = search_query_set;
    }

    public String getRich_rating() {
        return rich_rating;
    }

    public void setRich_rating(String rich_rating) {
        this.rich_rating = rich_rating;
    }

    public String getNum_review() {
        return num_review;
    }

    public void setNum_review(String num_review) {
        this.num_review = num_review;
    }

    public String getCategory() {
        return category;
    }

    public void setCategory(String category) {
        this.category = category;
    }

    public String getSuper_address() {
        return super_address;
    }

    public void setSuper_address(String super_address) {
        this.super_address = super_address;
    }

    public String getDatatype_spec() {
        return datatype_spec;
    }

    public void setDatatype_spec(String datatype_spec) {
        this.datatype_spec = datatype_spec;
    }

    public String getPoi() {
        return poi;
    }

    public void setPoi(String poi) {
        this.poi = poi;
    }

    public String getCitycode() {
        return citycode;
    }

    public void setCitycode(String citycode) {
        this.citycode = citycode;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public String getParent() {
        return parent;
    }

    public void setParent(String parent) {
        this.parent = parent;
    }

    public int getChildType() {
        return childType;
    }

    public void setChildType(int childType) {
        this.childType = childType;
    }

    public String getTowardsAngle() {
        return towardsAngle;
    }

    public void setTowardsAngle(String towardsAngle) {
        this.towardsAngle = towardsAngle;
    }

    public String getFloorNo() {
        return floorNo;
    }

    public void setFloorNo(String floorNo) {
        this.floorNo = floorNo;
    }

    public int getEndPoiExtension() {
        return endPoiExtension;
    }

    public void setEndPoiExtension(int endPoiExtension) {
        this.endPoiExtension = endPoiExtension;
    }
}
