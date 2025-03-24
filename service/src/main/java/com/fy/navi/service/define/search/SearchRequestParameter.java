package com.fy.navi.service.define.search;

import com.autonavi.gbl.search.model.SearchRoadId;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;
import java.util.List;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * 搜索参数，按类型分组，方便管理
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
public final class SearchRequestParameter {
    // 是否是静默搜索，默认非静默搜索
    private boolean mIsSilentSearch = false;
    // 搜索关键字,预搜索，关键字搜索必传
    private String mKeyword;
    // 分页
    private int mPage = 1;
    // 分页大小
    private int mSize = 10;
    // gbl搜索类型
    @AutoMapConstant.SearchQueryType
    private String mQueryType;

    // 用户位置。必出
    private GeoPoint mUserLoc;
    // poi 位置，POI 搜索必传
    private GeoPoint mPoiLoc;
    // 城市
    private int mCity;
    // 搜索类型,HMI 自定义类型
    @AutoMapConstant.SearchType
    private int mSearchType;
    // 区域
    private int mAdCode;
    // poi id
    private String mPoiId;
    // 行业
    private String mIndustry = "";
    private String mCategory;

    // 沿途搜索:路线上坐标点(至少两个)使用;分隔，如: 经度;纬度;经度;纬度，在线必传参数 , 抽稀点信息
    private List<GeoPoint> mGeolinePointList;
    // 算路起点位置信息
    private GeoPoint mStartPoint;
    // 算路终点位置信息
    private GeoPoint mEndPoint;
    // 算路途经点列表
    private List<GeoPoint> mViaPointList;
    // 离线指定引导路径道路 ，离线搜索条件，离线搜索必填
    private ArrayList<SearchRoadId> mGuideRoads;
    // 在线、离线搜索过滤条件 ,其中沿途离线搜索必传参数
    private String mFilterCondition;
    // 是否正在导航中,用于设置请求沿途搜接口的场景,true-行中，false-行前,默认为行中
    private boolean mIsNaving = true;
    // 导航类型,1-骑行,4-步行,2-驾车，5-货车，9-摩托车 默认2
    private int mNaviType = 2;
    // 是否需要加油站价格信息
    private boolean mIsNeedGasPrice;
    // 必传。筛选回传参数，使用搜索结果中的SearchClassifyInfo.retainState值原样回传。
    private String mRetainState;
    // 用户筛选级别。1：用户一筛发起请求， 2：用户二筛发起请求， 3：用户发起三筛请求， 其他情况发起请求可不传, level 1、2、3互斥。
    private String mCheckedLevel;
    // 一筛参数，使用搜索结果中的SearchClassifyItemInfo.categoryInfoList值填充，多个选项使用加号（+）拼接。
    private String mClassifyV2Data;
    // 二筛参数，使用搜索结果中的SearchClassifyItemInfo.level2CategoryInfoList值填充。
    private String mClassifyV2Level2Data;
    // 三筛参数，使用搜索结果中的SearchClassifyItemInfo.level3CategoryInfoList值填充。
    private String mClassifyV2Level3Data;
    // 获取对角线坐标字符串
    private String mGeoobj;
    // 聚合搜索类型
    private int mType;
    // 搜索路径信息
    private Object mPathInfo;
    // 沿途批量搜索poiId
    private List<String> mPoiIdList;
    //POI详情搜索回传参数
    private SearchRetainParamInfo mRetainParam;

    public boolean isSilentSearch() {
        return mIsSilentSearch;
    }

    public String getKeyword() {
        return mKeyword;
    }

    public int getPage() {
        return mPage;
    }

    public int getSize() {
        return mSize;
    }

    public String getQueryType() {
        return mQueryType;
    }

    public GeoPoint getUserLoc() {
        return mUserLoc;
    }

    public GeoPoint getPoiLoc() {
        return mPoiLoc;
    }

    public int getCity() {
        return mCity;
    }

    public int getSearchType() {
        return mSearchType;
    }

    public int getAdCode() {
        return mAdCode;
    }

    public String getPoiId() {
        return mPoiId;
    }

    public String getIndustry() {
        return mIndustry;
    }

    public String getCategory() {
        return mCategory;
    }

    public List<GeoPoint> getGeolinePointList() {
        return mGeolinePointList;
    }

    public GeoPoint getStartPoint() {
        return mStartPoint;
    }

    public GeoPoint getEndPoint() {
        return mEndPoint;
    }

    public List<GeoPoint> getViaPointList() {
        return mViaPointList;
    }

    public ArrayList<SearchRoadId> getGuideRoads() {
        return mGuideRoads;
    }

    public boolean isNaving() {
        return mIsNaving;
    }

    public String getFilterCondition() {
        return mFilterCondition;
    }

    public int getNaviType() {
        return mNaviType;
    }

    public boolean isNeedGasPrice() {
        return mIsNeedGasPrice;
    }

    public String getRetainState() {
        return mRetainState;
    }

    public String getCheckedLevel() {
        return mCheckedLevel;
    }

    public String getClassifyV2Data() {
        return mClassifyV2Data;
    }

    public String getClassifyV2Level2Data() {
        return mClassifyV2Level2Data;
    }

    public String getClassifyV2Level3Data() {
        return mClassifyV2Level3Data;
    }

    public String getGeoobj() {
        return mGeoobj;
    }

    public int getType() {
        return mType;
    }

    public Object getPathInfo() {
        return mPathInfo;
    }

    public List<String> getPoiIdList() {
        return mPoiIdList;
    }

    public SearchRetainParamInfo getRetainParam() {
        return mRetainParam;
    }

    private SearchRequestParameter(final Builder builder) {
        this.mIsSilentSearch = builder.mIsSilentSearch;
        this.mKeyword = builder.mKeyword;
        this.mPage = builder.mPage;
        this.mSize = builder.mSize;
        this.mQueryType = builder.mQueryType;
        this.mUserLoc = builder.mUserLoc;
        this.mPoiLoc = builder.mPoiLoc;
        this.mCity = builder.mCity;
        this.mSearchType = builder.mSearchType;
        this.mAdCode = builder.mAdCode;
        this.mPoiId = builder.mPoiId;
        this.mIndustry = builder.mIndustry;
        this.mCategory = builder.mCategory;
        this.mGeolinePointList = builder.mGeolinePointList;
        this.mStartPoint = builder.mStartPoint;
        this.mEndPoint = builder.mEndPoint;
        this.mViaPointList = builder.mViaPointList;
        this.mGuideRoads = builder.mGuideRoads;
        this.mFilterCondition = builder.mFilterCondition;
        this.mIsNaving = builder.mIsNaving;
        this.mNaviType = builder.mNaviType;
        this.mIsNeedGasPrice = builder.mIsNeedGasPrice;
        this.mRetainState = builder.mRetainState;
        this.mCheckedLevel = builder.mCheckedLevel;
        this.mClassifyV2Data = builder.mClassifyV2Data;
        this.mClassifyV2Level2Data = builder.mClassifyV2Level2Data;
        this.mClassifyV2Level3Data = builder.mClassifyV2Level3Data;
        this.mGeoobj = builder.mGeoobj;
        this.mType = builder.mType;
        this.mPathInfo = builder.mPathInfo;
        this.mPoiIdList = builder.mPoiIdList;
        this.mRetainParam = builder.mRetainParam;
    }

    public static class Builder {
        private boolean mIsSilentSearch = false;
        private String mKeyword;
        private int mPage = 1;
        private int mSize = 10;
        private String mQueryType;
        private GeoPoint mUserLoc;
        private GeoPoint mPoiLoc;
        private int mCity;
        private int mSearchType;
        private int mAdCode;
        private String mPoiId;
        private String mIndustry = "";
        private String mCategory;
        private List<GeoPoint> mGeolinePointList;
        private GeoPoint mStartPoint;
        private GeoPoint mEndPoint;
        private List<GeoPoint> mViaPointList;
        private ArrayList<SearchRoadId> mGuideRoads;
        private String mFilterCondition;
        private boolean mIsNaving = true;
        private int mNaviType = 2;
        private boolean mIsNeedGasPrice;
        private String mRetainState;
        private String mCheckedLevel;
        private String mClassifyV2Data;
        private String mClassifyV2Level2Data;
        private String mClassifyV2Level3Data;
        private String mGeoobj;
        private int mType;
        private Object mPathInfo;
        private List<String> mPoiIdList;
        private SearchRetainParamInfo mRetainParam;

        /**
         * 是否静默搜索
         * @param isSilentSearch 是否静默搜索
         * @return Builder
         */
        public Builder isSilentSearch(final boolean isSilentSearch) {
            this.mIsSilentSearch = isSilentSearch;
            return this;
        }

        /**
         * 关键字
         * @param keyword 关键字
         * @return Builder
         */
        public Builder keyword(final String keyword) {
            this.mKeyword = keyword;
            return this;
        }

        /**
         * 分页
         * @param page 分页
         * @return Builder
         */
        public Builder page(final int page) {
            this.mPage = page;
            return this;
        }

        /**
         * 分页大小
         * @param size 分页大小
         * @return Builder
         */
        public Builder size(final int size) {
            this.mSize = size;
            return this;
        }

        /**
         * 查询类型
         * @param queryType 查询类型
         * @return Builder
         */
        public Builder queryType(final String queryType) {
            this.mQueryType = queryType;
            return this;
        }

        /**
         * 用户位置
         * @param userLoc 用户位置
         * @return Builder
         */
        public Builder userLoc(final GeoPoint userLoc) {
            this.mUserLoc = userLoc;
            return this;
        }

        /**
         * POI位置
         * @param poiLoc POI位置
         * @return Builder
         */
        public Builder poiLoc(final GeoPoint poiLoc) {
            this.mPoiLoc = poiLoc;
            return this;
        }

        /**
         * 城市
         * @param city 城市
         * @return Builder
         */
        public Builder city(final int city) {
            this.mCity = city;
            return this;
        }

        /**
         * 搜索类型
         * @param searchType 搜索类型
         * @return Builder
         */
        public Builder searchType(final int searchType) {
            this.mSearchType = searchType;
            return this;
        }

        /**
         * 行政区划代码
         * @param adCode 行政区划代码
         * @return Builder
         */
        public Builder adCode(final int adCode) {
            this.mAdCode = adCode;
            return this;
        }

        /**
         * POI ID
         * @param poiId POI ID
         * @return Builder
         */
        public Builder poiId(final String poiId) {
            this.mPoiId = poiId;
            return this;
        }

        /**
         * 行业类型
         * @param industry 行业类型
         * @return Builder
         */
        public Builder industry(final String industry) {
            this.mIndustry = industry;
            return this;
        }

        /**
         * 类别
         * @param category 类别
         * @return Builder
         */
        public Builder category(final String category) {
            this.mCategory = category;
            return this;
        }

        /**
         * 沿途搜索:路线上坐标点(至少两个)使用
         * @param geolinePointList 沿途搜索:路线上坐标点(至少两个)使用
         * @return Builder
         */
        public Builder geolinePointList(final List<GeoPoint> geolinePointList) {
            this.mGeolinePointList = geolinePointList;
            return this;
        }

        /**
         * 起点
         * @param startPoint 起点
         * @return Builder
         */
        public Builder startPoint(final GeoPoint startPoint) {
            this.mStartPoint = startPoint;
            return this;
        }

        /**
         * 终点
         * @param endPoint 终点
         * @return Builder
         */
        public Builder endPoint(final GeoPoint endPoint) {
            this.mEndPoint = endPoint;
            return this;
        }

        /**
         * 途径点列表
         * @param viaPointList 途径点列表
         * @return Builder
         */
        public Builder viaPointList(final List<GeoPoint> viaPointList) {
            this.mViaPointList = viaPointList;
            return this;
        }

        /**
         * 路线指南
         * @param guideRoads 路线指南
         * @return Builder
         */
        public Builder guideRoads(final ArrayList<SearchRoadId> guideRoads) {
            this.mGuideRoads = guideRoads;
            return this;
        }

        /**
         * 过滤条件
         * @param filterCondition 过滤条件
         * @return Builder
         */
        public Builder filterCondition(final String filterCondition) {
            this.mFilterCondition = filterCondition;
            return this;
        }

        /**
         * 是否规划导航路线
         * @param isNaving 是否规划导航路线
         * @return Builder
         */
        public Builder isNaving(final boolean isNaving) {
            this.mIsNaving = isNaving;
            return this;
        }

        /**
         * 导航类型
         * @param naviType 导航类型
         * @return Builder
         */
        public Builder naviType(final int naviType) {
            this.mNaviType = naviType;
            return this;
        }

        /**
         * 是否需要加油价格
         * @param isNeedGasPrice 是否需要加油价格
         * @return Builder
         */
        public Builder isNeedGasPrice(final boolean isNeedGasPrice) {
            this.mIsNeedGasPrice = isNeedGasPrice;
            return this;
        }

        /**
         * 必传。筛选回传参数
         * @param retainState 必传。筛选回传参数
         * @return Builder
         */
        public Builder retainState(final String retainState) {
            this.mRetainState = retainState;
            return this;
        }

        /**
         * 用户筛选级别
         * @param checkedLevel 用户筛选级别
         * @return Builder
         */
        public Builder checkedLevel(final String checkedLevel) {
            this.mCheckedLevel = checkedLevel;
            return this;
        }

        /**
         * 一筛参数
         * @param classifyV2Data 一筛参数
         * @return Builder
         */
        public Builder classifyV2Data(final String classifyV2Data) {
            this.mClassifyV2Data = classifyV2Data;
            return this;
        }

        /**
         * 二筛参数
         * @param classifyV2Level2Data 二筛参数
         * @return Builder
         */
        public Builder classifyV2Level2Data(final String classifyV2Level2Data) {
            this.mClassifyV2Level2Data = classifyV2Level2Data;
            return this;
        }

        /**
         * 三筛参数
         * @param classifyV2Level3Data 三筛参数
         * @return Builder
         */
        public Builder classifyV2Level3Data(final String classifyV2Level3Data) {
            this.mClassifyV2Level3Data = classifyV2Level3Data;
            return this;
        }

        /**
         * 对角线坐标字符串
         * @param geoobj 对角线坐标字符串
         * @return Builder
         */
        public Builder geoobj(final String geoobj) {
            this.mGeoobj = geoobj;
            return this;
        }

        /**
         * 聚合搜索类型
         * @param type 聚合搜索类型
         * @return Builder
         */
        public Builder type(final int type) {
            this.mType = type;
            return this;
        }

        /**
         * 搜索路径信息
         * @param pathInfo 搜索路径信息
         * @return Builder
         */
        public Builder pathInfo(final Object pathInfo) {
            this.mPathInfo = pathInfo;
            return this;
        }

        /**
         * 沿途批量搜索poiId
         * @param poiIdList 沿途批量搜索poiId
         * @return Builder
         */
        public Builder poiIdList(final List<String> poiIdList) {
            this.mPoiIdList = poiIdList;
            return this;
        }

        /**
         * POI详情搜索回传参数
         * @param retainParam POI详情搜索回传参数
         * @return Builder
         */
        public Builder searchRetainParam(final SearchRetainParamInfo retainParam) {
            this.mRetainParam = retainParam;
            return this;
        }

        /**
         * 构建SearchRequestParameter对象
         * @return SearchRequestParameter
         */
        public SearchRequestParameter build() {
            return new SearchRequestParameter(this);
        }
    }
}