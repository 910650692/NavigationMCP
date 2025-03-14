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
 * 搜索参数，按类型分组，方便管理
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
public class SearchRequestParameter {
    // 是否是静默搜索，默认非静默搜索
    private boolean isSilentSearch = false;
    // 搜索关键字,预搜索，关键字搜索必传
    private String keyword;
    // 分页
    private int page = 1;
    // 分页大小
    private int size = 10;
    // gbl搜索类型
    @AutoMapConstant.SearchQueryType
    private String queryType;

    // 用户位置。必出
    private GeoPoint userLoc;
    // poi 位置，POI 搜索必传
    private GeoPoint poiLoc;
    // 城市
    private int city;
    // 搜索类型,HMI 自定义类型
    @AutoMapConstant.SearchType
    private int searchType;
    // 区域
    private int adCode;
    // poi id
    private String poiId;
    // 行业
    private String industry = "";
    private String category;

    // 沿途搜索:路线上坐标点(至少两个)使用;分隔，如: 经度;纬度;经度;纬度，在线必传参数 , 抽稀点信息
    private List<GeoPoint> geolinePointList;
    // 算路起点位置信息
    private GeoPoint startPoint;
    // 算路终点位置信息
    private GeoPoint endPoint;
    // 算路途经点列表
    private List<GeoPoint> viaPointList;
    // 离线指定引导路径道路 ，离线搜索条件，离线搜索必填
    private ArrayList<SearchRoadId> guideRoads;
    // 在线、离线搜索过滤条件 ,其中沿途离线搜索必传参数
    private String filterCondition;
    // 是否正在导航中,用于设置请求沿途搜接口的场景,true-行中，false-行前,默认为行中
    private boolean isNaving = true;
    // 导航类型,1-骑行,4-步行,2-驾车，5-货车，9-摩托车 默认2
    private int naviType = 2;
    // 是否需要加油站价格信息
    private boolean isNeedGasPrice;
    // 必传。筛选回传参数，使用搜索结果中的SearchClassifyInfo.retainState值原样回传。
    private String retainState;
    // 用户筛选级别。1：用户一筛发起请求， 2：用户二筛发起请求， 3：用户发起三筛请求， 其他情况发起请求可不传, level 1、2、3互斥。
    private String checkedLevel;
    // 一筛参数，使用搜索结果中的SearchClassifyItemInfo.categoryInfoList值填充，多个选项使用加号（+）拼接。
    private String classifyV2Data;
    // 二筛参数，使用搜索结果中的SearchClassifyItemInfo.level2CategoryInfoList值填充。
    private String classifyV2Level2Data;
    // 三筛参数，使用搜索结果中的SearchClassifyItemInfo.level3CategoryInfoList值填充。
    private String classifyV2Level3Data;
    // 获取对角线坐标字符串
    private String geoobj;
    // 聚合搜索类型
    private int type;
    // 搜索路径信息
    private Object pathInfo;
    // 沿途批量搜索poiId
    private List<String> poiIdList;
    //POI详情搜索回传参数
    private SearchRetainParamInfo retainParam;

    private SearchRequestParameter(Builder builder) {
        this.isSilentSearch = builder.isSilentSearch;
        this.keyword = builder.keyword;
        this.page = builder.page;
        this.size = builder.size;
        this.queryType = builder.queryType;
        this.userLoc = builder.userLoc;
        this.poiLoc = builder.poiLoc;
        this.city = builder.city;
        this.searchType = builder.searchType;
        this.adCode = builder.adCode;
        this.poiId = builder.poiId;
        this.industry = builder.industry;
        this.category = builder.category;
        this.geolinePointList = builder.geolinePointList;
        this.startPoint = builder.startPoint;
        this.endPoint = builder.endPoint;
        this.viaPointList = builder.viaPointList;
        this.guideRoads = builder.guideRoads;
        this.filterCondition = builder.filterCondition;
        this.isNaving = builder.isNaving;
        this.naviType = builder.naviType;
        this.isNeedGasPrice = builder.isNeedGasPrice;
        this.retainState = builder.retainState;
        this.checkedLevel = builder.checkedLevel;
        this.classifyV2Data = builder.classifyV2Data;
        this.classifyV2Level2Data = builder.classifyV2Level2Data;
        this.classifyV2Level3Data = builder.classifyV2Level3Data;
        this.geoobj = builder.geoobj;
        this.type = builder.type;
        this.pathInfo = builder.pathInfo;
        this.poiIdList = builder.poiIdList;
        this.retainParam = builder.retainParam;
    }

    public static class Builder {
        private boolean isSilentSearch = false;
        private String keyword;
        private int page = 1;
        private int size = 10;
        private String queryType;
        private GeoPoint userLoc;
        private GeoPoint poiLoc;
        private int city;
        private int searchType;
        private int adCode;
        private String poiId;
        private String industry = "";
        private String category;
        private List<GeoPoint> geolinePointList;
        private GeoPoint startPoint;
        private GeoPoint endPoint;
        private List<GeoPoint> viaPointList;
        private ArrayList<SearchRoadId> guideRoads;
        private String filterCondition;
        private boolean isNaving = true;
        private int naviType = 2;
        private boolean isNeedGasPrice;
        private String retainState;
        private String checkedLevel;
        private String classifyV2Data;
        private String classifyV2Level2Data;
        private String classifyV2Level3Data;
        private String geoobj;
        private int type;
        private Object pathInfo;
        private List<String> poiIdList;
        private SearchRetainParamInfo retainParam;

        public Builder isSilentSearch(boolean isSilentSearch) {
            this.isSilentSearch = isSilentSearch;
            return this;
        }

        public Builder keyword(String keyword) {
            this.keyword = keyword;
            return this;
        }

        public Builder page(int page) {
            this.page = page;
            return this;
        }

        public Builder size(int size) {
            this.size = size;
            return this;
        }

        public Builder queryType(String queryType) {
            this.queryType = queryType;
            return this;
        }

        public Builder userLoc(GeoPoint userLoc) {
            this.userLoc = userLoc;
            return this;
        }

        public Builder poiLoc(GeoPoint poiLoc) {
            this.poiLoc = poiLoc;
            return this;
        }

        public Builder city(int city) {
            this.city = city;
            return this;
        }

        public Builder searchType(int searchType) {
            this.searchType = searchType;
            return this;
        }

        public Builder adCode(int adCode) {
            this.adCode = adCode;
            return this;
        }

        public Builder poiId(String poiId) {
            this.poiId = poiId;
            return this;
        }

        public Builder industry(String industry) {
            this.industry = industry;
            return this;
        }

        public Builder category(String category) {
            this.category = category;
            return this;
        }

        public Builder geolinePointList(List<GeoPoint> geolinePointList) {
            this.geolinePointList = geolinePointList;
            return this;
        }

        public Builder startPoint(GeoPoint startPoint) {
            this.startPoint = startPoint;
            return this;
        }

        public Builder endPoint(GeoPoint endPoint) {
            this.endPoint = endPoint;
            return this;
        }

        public Builder viaPointList(List<GeoPoint> viaPointList) {
            this.viaPointList = viaPointList;
            return this;
        }

        public Builder guideRoads(ArrayList<SearchRoadId> guideRoads) {
            this.guideRoads = guideRoads;
            return this;
        }

        public Builder filterCondition(String filterCondition) {
            this.filterCondition = filterCondition;
            return this;
        }

        public Builder isNaving(boolean isNaving) {
            this.isNaving = isNaving;
            return this;
        }

        public Builder naviType(int naviType) {
            this.naviType = naviType;
            return this;
        }

        public Builder isNeedGasPrice(boolean isNeedGasPrice) {
            this.isNeedGasPrice = isNeedGasPrice;
            return this;
        }

        public Builder retainState(String retainState) {
            this.retainState = retainState;
            return this;
        }

        public Builder checkedLevel(String checkedLevel) {
            this.checkedLevel = checkedLevel;
            return this;
        }

        public Builder classifyV2Data(String classifyV2Data) {
            this.classifyV2Data = classifyV2Data;
            return this;
        }

        public Builder classifyV2Level2Data(String classifyV2Level2Data) {
            this.classifyV2Level2Data = classifyV2Level2Data;
            return this;
        }

        public Builder classifyV2Level3Data(String classifyV2Level3Data) {
            this.classifyV2Level3Data = classifyV2Level3Data;
            return this;
        }

        public Builder geoobj(String geoobj) {
            this.geoobj = geoobj;
            return this;
        }

        public Builder type(int type) {
            this.type = type;
            return this;
        }

        public Builder pathInfo(Object pathInfo) {
            this.pathInfo = pathInfo;
            return this;
        }

        public Builder poiIdList(List<String> poiIdList) {
            this.poiIdList = poiIdList;
            return this;
        }

        public Builder searchRetainParam(SearchRetainParamInfo retainParam) {
            this.retainParam = retainParam;
            return this;
        }

        public SearchRequestParameter build() {
            return new SearchRequestParameter(this);
        }
    }
}