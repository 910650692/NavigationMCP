package com.sgm.navi.service.greendao.favorite;

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
    private Long mId;

    @Unique
    @Property(nameInDb = "itemId")
    private String mItemId; // 收藏点唯一码, 由AutoSDK内部生成

    @Property(nameInDb = "commonName")
    private int mCommonName;   // 收藏点类型（1家，2公司，3常去地址，0普通收藏点）

    @Property(nameInDb = "tag")
    private String mTag; // 附加标签

    @Property(nameInDb = "type")
    private String mType;  // 类型

    @Property(nameInDb = "newType")
    private String mNewType; //  新类型，预留

    @Property(nameInDb = "customName")   // 自定义名称 重命名时编辑的字段
    private String mCustomName;

    @Property(nameInDb = "classification")
    private String mClassification; //  类别

    @Property(nameInDb = "topTime")
    private long mTopTime; // 置顶操作内部更新时间

    @Property(nameInDb = "pid")
    private String mPid; // 父POI的Id

    @Property(nameInDb = "distance")
    private String mDistance;

    @Property(nameInDb = "name")
    private String mName;       //  名称

    @Property(nameInDb = "phone")
    private String mPhone;       // 电话

    @Property(nameInDb = "cityName")
    private String mCityName;   // 城市区号

    @Property(nameInDb = "cityCode")
    private String mCityCode;   // 城市区号

    @Property(nameInDb = "address")
    private String mAddress;   // 地址

    @Property(nameInDb = "point_x")
    private double mPointX;     // 经纬度坐标

    @Property(nameInDb = "point_y")
    private double mPointY;     // 经纬度坐标

    @Property(nameInDb = "point_x_arrive")
    private double mPointXArrive;     // 导航经纬度坐标

    @Property(nameInDb = "point_y_arrive")
    private double mPointYArrive;     // 导航经纬度坐标

    @Property(nameInDb = "updateTime")
    private Date mUpdateTime;   // 数据更新时间

    @Keep
    public Favorite(final Long id, final String itemId, final int commonName, final String tag, final String type,
            final String newType, final String customName, final String classification, final long topTime,
            final String pid, final String distance, final String name, final String phone, final String cityName,
            final String cityCode, final String address, final double pointX, final double pointY,
            final double pointXArrive, final double pointYArrive, final Date updateTime) {
        this.mId = id;
        this.mItemId = itemId;
        this.mCommonName = commonName;
        this.mTag = tag;
        this.mType = type;
        this.mNewType = newType;
        this.mCustomName = customName;
        this.mClassification = classification;
        this.mTopTime = topTime;
        this.mPid = pid;
        this.mDistance = distance;
        this.mName = name;
        this.mPhone = phone;
        this.mCityName = cityName;
        this.mCityCode = cityCode;
        this.mAddress = address;
        this.mPointX = pointX;
        this.mPointY = pointY;
        this.mPointXArrive = pointXArrive;
        this.mPointYArrive = pointYArrive;
        this.mUpdateTime = updateTime;
    }

    @Generated(hash = 459811785)
    public Favorite() {
    }

    public Long getMId() {
        return this.mId;
    }

    public void setMId(final Long id) {
        this.mId = id;
    }

    public String getMItemId() {
        return this.mItemId;
    }

    public void setMItemId(final String itemId) {
        this.mItemId = itemId;
    }

    public int getMCommonName() {
        return this.mCommonName;
    }

    public void setMCommonName(final int commonName) {
        this.mCommonName = commonName;
    }

    public String getMTag() {
        return this.mTag;
    }

    public void setMTag(final String tag) {
        this.mTag = tag;
    }

    public String getMType() {
        return this.mType;
    }

    public void setMType(final String type) {
        this.mType = type;
    }

    public String getMNewType() {
        return this.mNewType;
    }

    public void setMNewType(final String newType) {
        this.mNewType = newType;
    }

    public String getMCustomName() {
        return this.mCustomName;
    }

    public void setMCustomName(final String customName) {
        this.mCustomName = customName;
    }

    public String getMClassification() {
        return this.mClassification;
    }

    public void setMClassification(final String classification) {
        this.mClassification = classification;
    }

    public long getMTopTime() {
        return this.mTopTime;
    }

    public void setMTopTime(final long topTime) {
        this.mTopTime = topTime;
    }

    public String getMPid() {
        return this.mPid;
    }

    public void setMPid(final String pid) {
        this.mPid = pid;
    }

    public String getMDistance() {
        return this.mDistance;
    }

    public void setMDistance(final String distance) {
        this.mDistance = distance;
    }

    public String getMName() {
        return this.mName;
    }

    public void setMName(final String name) {
        this.mName = name;
    }

    public String getMPhone() {
        return this.mPhone;
    }

    public void setMPhone(final String phone) {
        this.mPhone = phone;
    }

    public String getMCityName() {
        return this.mCityName;
    }

    public void setMCityName(final String cityName) {
        this.mCityName = cityName;
    }

    public String getMCityCode() {
        return this.mCityCode;
    }

    public void setMCityCode(final String cityCode) {
        this.mCityCode = cityCode;
    }

    public String getMAddress() {
        return this.mAddress;
    }

    public void setMAddress(final String address) {
        this.mAddress = address;
    }

    public double getMPointX() {
        return this.mPointX;
    }

    public void setMPointX(final double pointX) {
        this.mPointX = pointX;
    }

    public double getMPointY() {
        return this.mPointY;
    }

    public void setMPointY(final double pointY) {
        this.mPointY = pointY;
    }

    public double getMPointXArrive() {
        return this.mPointXArrive;
    }

    public void setMPointXArrive(final double pointXArrive) {
        this.mPointXArrive = pointXArrive;
    }

    public double getMPointYArrive() {
        return this.mPointYArrive;
    }

    public void setMPointYArrive(final double pointYArrive) {
        this.mPointYArrive = pointYArrive;
    }

    public Date getMUpdateTime() {
        return this.mUpdateTime;
    }

    public void setMUpdateTime(final Date updateTime) {
        this.mUpdateTime = updateTime;
    }
}
