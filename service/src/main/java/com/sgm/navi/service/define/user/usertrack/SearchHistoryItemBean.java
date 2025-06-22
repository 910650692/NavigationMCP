package com.sgm.navi.service.define.user.usertrack;

import java.util.ArrayList;


public class SearchHistoryItemBean {

    private String mName; // 记录搜索POI名字或者搜索词
    private String mPoiid; // POI唯一ID
    private String mId;
    private int mType;
    private int mDatatype;
    private double mX;
    private double mY;
    private double mXentr;
    private double mYentr;
    private long mUpdateTime; // 数据更新时间，单位秒
    private int mHistoryType;
    private int mIconinfo;
    private String mAdcode;
    private String mDistrict;
    private String mAddress;
    private String mPoiTag;
    private String mFuncText;
    private String mShortName;
    private String mDisplayInfo;
    private String mSearchQuery;
    private String mTerminals;
    private String mIgnoreDistrict;
    private ArrayList<String> mSearchTag;
    private ArrayList<String> mSearchQuerySet;
    private String mRichRating;
    private String mNumReview;
    private String mCategory;
    private String mSuperAddress;
    private String mDatatypeSpec; // 记录对应记录类型，具体Icon资源需要上层关联
    private String mPoi;
    private String mCitycode;
    private String mVersion;
    private String mParent;
    private int mChildType;
    private String mTowardsAngle;
    private String mFloorNo;
    private int mEndPoiExtension;

    public String getName() {
        return mName;
    }

    public void setName(final String name) {
        this.mName = name;
    }

    public String getPoiid() {
        return mPoiid;
    }

    public void setPoiid(final String poiid) {
        this.mPoiid = poiid;
    }

    public String getId() {
        return mId;
    }

    public void setId(final String id) {
        this.mId = id;
    }

    public int getType() {
        return mType;
    }

    public void setType(final int type) {
        this.mType = type;
    }

    public int getDatatype() {
        return mDatatype;
    }

    public void setDatatype(final int datatype) {
        this.mDatatype = datatype;
    }

    public double getX() {
        return mX;
    }

    public void setX(final double x) {
        this.mX = x;
    }

    public double getY() {
        return mY;
    }

    public void setY(final double y) {
        this.mY = y;
    }

    public double getXentr() {
        return mXentr;
    }

    public void setXentr(final double xentr) {
        this.mXentr = xentr;
    }

    public double getYentr() {
        return mYentr;
    }

    public void setYentr(final double yentr) {
        this.mYentr = yentr;
    }

    public long getUpdateTime() {
        return mUpdateTime;
    }

    public void setUpdateTime(final long updateTime) {
        this.mUpdateTime = updateTime;
    }

    public int getHistoryType() {
        return mHistoryType;
    }

    public void setHistoryType(final int historyType) {
        this.mHistoryType = historyType;
    }

    public int getIconinfo() {
        return mIconinfo;
    }

    public void setIconinfo(final int iconinfo) {
        this.mIconinfo = iconinfo;
    }

    public String getAdcode() {
        return mAdcode;
    }

    public void setAdcode(final String adcode) {
        this.mAdcode = adcode;
    }

    public String getDistrict() {
        return mDistrict;
    }

    public void setDistrict(final String district) {
        this.mDistrict = district;
    }

    public String getAddress() {
        return mAddress;
    }

    public void setAddress(final String address) {
        this.mAddress = address;
    }

    public String getPoiTag() {
        return mPoiTag;
    }

    public void setPoiTag(final String poiTag) {
        this.mPoiTag = poiTag;
    }

    public String getFuncText() {
        return mFuncText;
    }

    public void setFuncText(final String funcText) {
        this.mFuncText = funcText;
    }

    public String getShortName() {
        return mShortName;
    }

    public void setShortName(final String shortName) {
        this.mShortName = shortName;
    }

    public String getDisplayInfo() {
        return mDisplayInfo;
    }

    public void setDisplayInfo(final String displayInfo) {
        this.mDisplayInfo = displayInfo;
    }

    public String getSearchQuery() {
        return mSearchQuery;
    }

    public void setSearchQuery(final String searchQuery) {
        this.mSearchQuery = searchQuery;
    }

    public String getTerminals() {
        return mTerminals;
    }

    public void setTerminals(final String terminals) {
        this.mTerminals = terminals;
    }

    public String getIgnoreDistrict() {
        return mIgnoreDistrict;
    }

    public void setIgnoreDistrict(final String ignoreDistrict) {
        this.mIgnoreDistrict = ignoreDistrict;
    }

    public ArrayList<String> getSearchTag() {
        return mSearchTag;
    }

    public void setSearchTag(final ArrayList<String> searchTag) {
        this.mSearchTag = searchTag;
    }

    public ArrayList<String> getSearchQuerySet() {
        return mSearchQuerySet;
    }

    public void setSearchQuerySet(final ArrayList<String> searchQuerySet) {
        this.mSearchQuerySet = searchQuerySet;
    }

    public String getRichRating() {
        return mRichRating;
    }

    public void setRichRating(final String richRating) {
        this.mRichRating = richRating;
    }

    public String getNumReview() {
        return mNumReview;
    }

    public void setNumReview(final String numReview) {
        this.mNumReview = numReview;
    }

    public String getCategory() {
        return mCategory;
    }

    public void setCategory(final String category) {
        this.mCategory = category;
    }

    public String getSuperAddress() {
        return mSuperAddress;
    }

    public void setSuperAddress(final String superAddress) {
        this.mSuperAddress = superAddress;
    }

    public String getDatatypeSpec() {
        return mDatatypeSpec;
    }

    public void setDatatypeSpec(final String datatypeSpec) {
        this.mDatatypeSpec = datatypeSpec;
    }

    public String getPoi() {
        return mPoi;
    }

    public void setPoi(final String poi) {
        this.mPoi = poi;
    }

    public String getCitycode() {
        return mCitycode;
    }

    public void setCitycode(final String citycode) {
        this.mCitycode = citycode;
    }

    public String getVersion() {
        return mVersion;
    }

    public void setVersion(final String version) {
        this.mVersion = version;
    }

    public String getParent() {
        return mParent;
    }

    public void setParent(final String parent) {
        this.mParent = parent;
    }

    public int getChildType() {
        return mChildType;
    }

    public void setChildType(final int childType) {
        this.mChildType = childType;
    }

    public String getTowardsAngle() {
        return mTowardsAngle;
    }

    public void setTowardsAngle(final String towardsAngle) {
        this.mTowardsAngle = towardsAngle;
    }

    public String getFloorNo() {
        return mFloorNo;
    }

    public void setFloorNo(final String floorNo) {
        this.mFloorNo = floorNo;
    }

    public int getEndPoiExtension() {
        return mEndPoiExtension;
    }

    public void setEndPoiExtension(final int endPoiExtension) {
        this.mEndPoiExtension = endPoiExtension;
    }
}
