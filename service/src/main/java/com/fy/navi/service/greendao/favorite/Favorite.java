package com.fy.navi.service.greendao.favorite;

import org.greenrobot.greendao.annotation.Entity;
import org.greenrobot.greendao.annotation.Generated;
import org.greenrobot.greendao.annotation.Id;
import org.greenrobot.greendao.annotation.Keep;
import org.greenrobot.greendao.annotation.Property;
import org.greenrobot.greendao.annotation.Unique;

import java.util.Date;

@Entity
public class Favorite {

    @Id(autoincrement = true)
    public Long id;

    @Unique
    @Property(nameInDb = "itemId")
    public String itemId; // 收藏点唯一码, 由AutoSDK内部生成

    @Property(nameInDb = "commonName")
    public int commonName;   // 收藏点类型（1家，2公司，3常去地址，0普通收藏点）

    @Property(nameInDb = "tag")
    public String tag; // 附加标签

    @Property(nameInDb = "type")
    public String type;  // 类型

    @Property(nameInDb = "newType")
    public String newType; //  新类型，预留

    @Property(nameInDb = "customName")   // 自定义名称 重命名时编辑的字段
    public String customName;

    @Property(nameInDb = "classification")
    public String classification; //  类别

    @Property(nameInDb = "topTime")
    public long topTime; // 置顶操作内部更新时间

    @Property(nameInDb = "pid")
    public String pid; // 父POI的Id

    @Property(nameInDb = "distance")
    public String distance;

    @Property(nameInDb = "name")
    public String name;       //  名称

    @Property(nameInDb = "phone")
    public String phone;       // 电话

    @Property(nameInDb = "cityName")
    public String cityName;   // 城市区号

    @Property(nameInDb = "cityCode")
    public String cityCode;   // 城市区号

    @Property(nameInDb = "address")
    public String address;   // 地址

    @Property(nameInDb = "point_x")
    public double point_x;     // 经纬度坐标

    @Property(nameInDb = "point_y")
    public double point_y;     // 经纬度坐标

    @Property(nameInDb = "point_x_arrive")
    public double point_x_arrive;     // 导航经纬度坐标

    @Property(nameInDb = "point_y_arrive")
    public double point_y_arrive;     // 导航经纬度坐标

    @Property(nameInDb = "updateTime")
    public Date updateTime;   // 数据更新时间

    @Generated(hash = 1401461682)
    public Favorite(Long id, String itemId, int commonName, String tag, String type,
            String newType, String customName, String classification, long topTime,
            String pid, String distance, String name, String phone, String cityName,
            String cityCode, String address, double point_x, double point_y,
            double point_x_arrive, double point_y_arrive, Date updateTime) {
        this.id = id;
        this.itemId = itemId;
        this.commonName = commonName;
        this.tag = tag;
        this.type = type;
        this.newType = newType;
        this.customName = customName;
        this.classification = classification;
        this.topTime = topTime;
        this.pid = pid;
        this.distance = distance;
        this.name = name;
        this.phone = phone;
        this.cityName = cityName;
        this.cityCode = cityCode;
        this.address = address;
        this.point_x = point_x;
        this.point_y = point_y;
        this.point_x_arrive = point_x_arrive;
        this.point_y_arrive = point_y_arrive;
        this.updateTime = updateTime;
    }

    @Generated(hash = 459811785)
    public Favorite() {
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getItemId() {
        return itemId;
    }

    public void setItemId(String itemId) {
        this.itemId = itemId;
    }

    public int getCommonName() {
        return commonName;
    }

    public void setCommonName(int commonName) {
        this.commonName = commonName;
    }

    public String getTag() {
        return tag;
    }

    public void setTag(String tag) {
        this.tag = tag;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getNewType() {
        return newType;
    }

    public void setNewType(String newType) {
        this.newType = newType;
    }

    public String getCustomName() {
        return customName;
    }

    public void setCustomName(String customName) {
        this.customName = customName;
    }

    public String getClassification() {
        return classification;
    }

    public void setClassification(String classification) {
        this.classification = classification;
    }

    public long getTopTime() {
        return topTime;
    }

    public void setTopTime(long topTime) {
        this.topTime = topTime;
    }

    public String getPid() {
        return pid;
    }

    public void setPid(String pid) {
        this.pid = pid;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getPhone() {
        return phone;
    }

    public void setPhone(String phone) {
        this.phone = phone;
    }

    public String getCityCode() {
        return cityCode;
    }

    public void setCityCode(String cityCode) {
        this.cityCode = cityCode;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public String getCityName() {
        return cityName;
    }

    public void setCityName(String cityName) {
        this.cityName = cityName;
    }

    public double getPoint_x() {
        return point_x;
    }

    public void setPoint_x(double point_x) {
        this.point_x = point_x;
    }

    public double getPoint_y() {
        return point_y;
    }

    public void setPoint_y(double point_y) {
        this.point_y = point_y;
    }

    public double getPoint_x_arrive() {
        return point_x_arrive;
    }

    public void setPoint_x_arrive(double point_x_arrive) {
        this.point_x_arrive = point_x_arrive;
    }

    public double getPoint_y_arrive() {
        return point_y_arrive;
    }

    public void setPoint_y_arrive(double point_y_arrive) {
        this.point_y_arrive = point_y_arrive;
    }

    public Date getUpdateTime() {
        return updateTime;
    }

    public void setUpdateTime(Date updateTime) {
        this.updateTime = updateTime;
    }

    public String getDistance() {
        return this.distance;
    }

    public void setDistance(String distance) {
        this.distance = distance;
    }
}
