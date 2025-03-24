package com.fy.navi.service.adapter.search.bls;


import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.autonavi.gbl.aosclient.model.GNavigationEtaqueryReqElecConstList;
import com.autonavi.gbl.aosclient.model.GNavigationEtaqueryReqElecConstListRangeEnergy;
import com.autonavi.gbl.aosclient.model.GNavigationEtaqueryReqStartPoints;
import com.autonavi.gbl.aosclient.model.GNavigationEtaqueryRequestParam;
import com.autonavi.gbl.common.model.RectDouble;
import com.autonavi.gbl.search.model.KeywordSearchIdqParam;
import com.autonavi.gbl.search.model.KeywordSearchRqbxyParam;
import com.autonavi.gbl.search.model.KeywordSearchTQueryParam;
import com.autonavi.gbl.search.model.SearchAggregateParam;
import com.autonavi.gbl.search.model.SearchAlongWayParam;
import com.autonavi.gbl.search.model.SearchDataType;
import com.autonavi.gbl.search.model.SearchEnrouteCategoryParam;
import com.autonavi.gbl.search.model.SearchEnrouteKeywordParam;
import com.autonavi.gbl.search.model.SearchEnrouteScene;
import com.autonavi.gbl.search.model.SearchPoiDetailParam;
import com.autonavi.gbl.search.model.SearchRetainParam;
import com.autonavi.gbl.search.model.SearchSuggestionParam;
import com.autonavi.gbl.search.model.SearchSuggestionQueryType;
import com.autonavi.gbl.search.model.SearchTipsCityInfo;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.search.SearchRequestParameter;
import com.fy.navi.service.define.search.SearchRetainParamInfo;
import com.fy.navi.service.define.search.SearchTipsCityLocalInfo;
import com.fy.navi.service.define.search.SearchTipsLocalInfo;
import com.fy.navi.service.define.utils.BevPowerCarUtils;

import java.util.ArrayList;
import java.util.List;

public final class SearchRequestParamV2 {
    private static final int DEFAULT_RESULT_MAX_COUNT = 10;
    private static final String DEFAULT_VALUE = "0.0";

    private SearchRequestParamV2() {
    }

    public static SearchRequestParamV2 getInstance() {
        return HelperHolder.INSTANCE;
    }

    private static final class HelperHolder {
        private static final SearchRequestParamV2 INSTANCE = new SearchRequestParamV2();
    }


    /**
     * 联想搜索
     *
     * @param searchRequestInfo SearchRequestParameter
     * @return SearchSuggestionParam
     */
    public SearchSuggestionParam convertToSearchSuggestionParamV2(final SearchRequestParameter searchRequestInfo) {
        final SearchSuggestionParam param = new SearchSuggestionParam();
        param.keyword = searchRequestInfo.getKeyword();
        param.city = searchRequestInfo.getAdCode();
        param.queryType = SearchSuggestionQueryType.General;
        param.type = 0;
        param.offlineParam.resultMaxCount = DEFAULT_RESULT_MAX_COUNT;
        param.switchParam.needVirtualTip = true;
        param.dataType = SearchDataType.Poi;
        param.userLoc.lon = searchRequestInfo.getUserLoc().getLon();
        param.userLoc.lat = searchRequestInfo.getUserLoc().getLat();
        param.switchParam.needAdcode = true;
        return param;
    }


    /**
     * 关键字
     *
     * @param searchRequestInfo SearchRequestParameter
     * @return KeywordSearchTQueryParam
     */
    public KeywordSearchTQueryParam convertToSearchKeywordParamV2(final SearchRequestParameter searchRequestInfo) {
        final KeywordSearchTQueryParam paramV2 = new KeywordSearchTQueryParam();
        paramV2.keywords = searchRequestInfo.getKeyword();
        paramV2.pageParam.pageSize = DEFAULT_RESULT_MAX_COUNT;
        paramV2.pageParam.pageNum = searchRequestInfo.getPage();
        paramV2.queryType = searchRequestInfo.getQueryType();
        paramV2.userLoc.lon = searchRequestInfo.getUserLoc().getLon();
        paramV2.userLoc.lat = searchRequestInfo.getUserLoc().getLat();
        paramV2.poiLoc.lon = searchRequestInfo.getUserLoc().getLon();
        paramV2.poiLoc.lat = searchRequestInfo.getUserLoc().getLat();
        paramV2.city = String.valueOf(searchRequestInfo.getAdCode());
        paramV2.geoObj = searchRequestInfo.getGeoobj();
        paramV2.offlineParam.resultMaxCount = DEFAULT_RESULT_MAX_COUNT;
        paramV2.customParam.classifyParam.retainState = searchRequestInfo.getRetainState();
        paramV2.customParam.classifyParam.checkedLevel = searchRequestInfo.getCheckedLevel();
        paramV2.customParam.classifyParam.classifyV2Data = searchRequestInfo.getClassifyV2Data();
        paramV2.customParam.classifyParam.classifyV2Level2Data = searchRequestInfo.getClassifyV2Level2Data();
        paramV2.customParam.classifyParam.claissfyV2Level3Data = searchRequestInfo.getClassifyV2Level3Data();

        return paramV2;
    }

    /**
     * 聚合搜索
     *
     * @param searchRequestInfo SearchRequestParameter
     * @return SearchAggregateParam
     */
    public SearchAggregateParam convertToSearchAggregateParamV2(final SearchRequestParameter searchRequestInfo) {
        final SearchAggregateParam paramV2 = new SearchAggregateParam();
        paramV2.type = searchRequestInfo.getType();
        paramV2.userLoc.lon = searchRequestInfo.getUserLoc().getLon();
        paramV2.userLoc.lat = searchRequestInfo.getUserLoc().getLat();
        paramV2.userCity = String.valueOf(searchRequestInfo.getAdCode());
        paramV2.viewRegion = new RectDouble();
        return paramV2;
    }

    /**
     * 顺路-品类搜索
     *
     * @param searchRequestInfo SearchRequestParameter
     * @return SearchEnrouteCategoryParam
     */
    public SearchEnrouteCategoryParam convertToSearchEnRouteCategoryParamV2(SearchRequestParameter searchRequestInfo) {
        SearchEnrouteCategoryParam paramV2 = new SearchEnrouteCategoryParam();
        paramV2.userLoc.lon = searchRequestInfo.getUserLoc().getLon();
        paramV2.userLoc.lat = searchRequestInfo.getUserLoc().getLat();
        // paramV2.category = searchRequestInfo.getCategory(); //需要先请求品类搜索拿到结果
        paramV2.naviScene = SearchEnrouteScene.BeforeNavi;
        return paramV2;
    }

    /**
     * 顺路-关键字搜索
     *
     * @param searchRequestInfo SearchRequestParameter
     * @return SearchEnrouteKeywordParam
     */
    public SearchEnrouteKeywordParam convertToSearchEnRouteKeywordParamV2(final SearchRequestParameter searchRequestInfo) {
        final SearchEnrouteKeywordParam paramV2 = new SearchEnrouteKeywordParam();
        paramV2.keyword = searchRequestInfo.getKeyword();
        paramV2.userLoc.lon = searchRequestInfo.getUserLoc().getLon();
        paramV2.userLoc.lat = searchRequestInfo.getUserLoc().getLat();
        paramV2.naviScene = SearchEnrouteScene.BeforeNavi;
        Logger.d("NaviApp_Search_Service", "convertToSearchEnRouteKeywordParamV2 keyword: " + paramV2.keyword
                + " userLoc: " + paramV2.userLoc.lat + "," + paramV2.userLoc.lon
                + " naviScene: " + paramV2.naviScene);
        return paramV2;
    }

    /**
     * Poi详情搜索
     *
     * @param searchRequestInfo SearchRequestParameter
     * @return SearchPoiDetailParam
     */
    public SearchPoiDetailParam convertToSearchPoiDetailParamV2(final SearchRequestParameter searchRequestInfo) {
        final SearchPoiDetailParam param = new SearchPoiDetailParam();
        param.adcode = searchRequestInfo.getAdCode();
        param.poiId = searchRequestInfo.getPoiId();
        param.userLoc.lon = searchRequestInfo.getUserLoc().getLon();
        param.userLoc.lat = searchRequestInfo.getUserLoc().getLat();
        //关键字搜回传数据
        final SearchRetainParam retainParam = new SearchRetainParam();
        convertToSearchRetainParam(searchRequestInfo.getRetainParam(), retainParam);
        param.retainParam = retainParam;
        param.itemList = null;
        return param;
    }

    /**
     * Poi筛选搜索
     *
     * @param searchRetainParamInfo SearchRetainParamInfo
     * @param retainParam 由SDK传入，直接获取
     */
    private void convertToSearchRetainParam(final SearchRetainParamInfo searchRetainParamInfo, final SearchRetainParam retainParam) {
        if (searchRetainParamInfo != null) {
            retainParam.keywordBizType = searchRetainParamInfo.getKeywordBizType();
            final SearchTipsLocalInfo tipsLocalInfo = searchRetainParamInfo.getSearchTipsLocalInfo();
            retainParam.tipsInfo.keyword = tipsLocalInfo.getKeyword();
            retainParam.tipsInfo.cityName = tipsLocalInfo.getCityName();
            retainParam.tipsInfo.sceneType = tipsLocalInfo.getSceneType();
            retainParam.tipsInfo.retainState = tipsLocalInfo.getRetainState();
            retainParam.tipsInfo.optionalKeywordList = tipsLocalInfo.getOptionalKeywordList();
            for (SearchTipsCityLocalInfo cityLocalInfo : tipsLocalInfo.getOptionalCityList()) {
                final SearchTipsCityInfo searchTipsCityInfo = new SearchTipsCityInfo();
                searchTipsCityInfo.adcode = cityLocalInfo.getAdcode();
                searchTipsCityInfo.name = cityLocalInfo.getName();
                searchTipsCityInfo.index = cityLocalInfo.getIndex();
                retainParam.tipsInfo.optionalCityList.add(searchTipsCityInfo);
            }
        }
    }

    /**
     * Poi id 搜索
     *
     * @param searchRequestInfo SearchRequestParameter
     * @return KeywordSearchIdqParam
     */
    public KeywordSearchIdqParam convertToKeywordSearchIdqParam(final SearchRequestParameter searchRequestInfo) {
        final KeywordSearchIdqParam param = new KeywordSearchIdqParam();
        param.id = searchRequestInfo.getPoiId();
        param.userLoc.lon = searchRequestInfo.getUserLoc().getLon();
        param.userLoc.lat = searchRequestInfo.getUserLoc().getLat();
        return param;
    }

    /**
     * 沿途搜索
     *
     * @param searchRequestInfo SearchRequestParameter
     * @return SearchAlongWayParam
     */
    public SearchAlongWayParam convertToAlongWaySearchIdqParam(final SearchRequestParameter searchRequestInfo) {
        final SearchAlongWayParam param = new SearchAlongWayParam();
        // 路线上坐标点(至少两个)使用;分隔，如: 经度;纬度;经度;纬度，在线必传参数
        final String flag = ";";
        final String geoline = convertPointList2String(searchRequestInfo.getGeolinePointList(), flag);

        // 沿途搜起点、途经点坐标、终点，用分号间隔(最后不加分号)
        List<GeoPoint> routePointList = searchRequestInfo.getViaPointList();
        if (routePointList == null) {
            routePointList = new ArrayList<>();
        }

        routePointList.add(searchRequestInfo.getEndPoint());
        routePointList.add(0, searchRequestInfo.getStartPoint());
        final String routePoints = convertPointList2String(routePointList, flag);

        final SearchAlongWayParam para = new SearchAlongWayParam();
        // 城市编码，离线搜索条件, 离线搜索必填
        para.adcode = searchRequestInfo.getAdCode();
        // 用户位置，经度,纬度 离线搜索用
        para.user_loc.lon = searchRequestInfo.getUserLoc().getLon();
        para.user_loc.lat = searchRequestInfo.getUserLoc().getLat();

        // 在线沿路径道路点信息字符串 ，在线搜索条件，在线搜索必填
        para.geoline = geoline;
        // 在线搜索筛选条件
        para.auto_attr_filter = searchRequestInfo.getFilterCondition();
        // 是否需要是否需要到eta，默认为false，在线参数必填
        para.need_eta = true;
        // 是否需要到达点信息，默认为false，在线参数
        para.need_naviinfo = true;
        // 起点、途经点坐标、终点，用分号间隔，必填字段，在线参数
        para.routepoints = routePoints;
        // 请求沿途搜接口的场景,1-行中，2-行前,必填，默认1
        para.navi_scene = searchRequestInfo.isNaving() ? 1 : 2;
        // 导航类型,1-骑行,4-步行,2-驾车，5-货车，9-摩托车 必填 默认2
        para.navi_type = searchRequestInfo.getNaviType();

        para.keyword = searchRequestInfo.getKeyword();
        // 离线指定引导路径道路,离线搜索必填
        para.guideRoads = searchRequestInfo.getGuideRoads();
        if (searchRequestInfo.getCategory() != null && !searchRequestInfo.getCategory().isEmpty()) {
            para.category = searchRequestInfo.getCategory();
        }
        // 下发路线的能耗信息
        para.contentoptions = 0x2000;
        para.route_range = 5000;
        //不压缩
        para.linkid_format = 0;
        //64位ID
        para.linkid_type = 3;
        para.need_gasprice = searchRequestInfo.isNeedGasPrice();
        return param;
    }

    /**
     * 周边搜索
     *
     * @param searchRequestInfo SearchRequestParameter
     * @return SearchAlongWayParam
     */
    public KeywordSearchRqbxyParam convertToAroundSearchParam(final SearchRequestParameter searchRequestInfo) {
        final KeywordSearchRqbxyParam paramV2 = new KeywordSearchRqbxyParam();
        paramV2.keywords = searchRequestInfo.getKeyword();
        paramV2.pageParam.pageSize = DEFAULT_RESULT_MAX_COUNT;
        paramV2.pageParam.pageNum = searchRequestInfo.getPage();
        paramV2.queryType = searchRequestInfo.getQueryType();
        paramV2.userLoc.lon = searchRequestInfo.getUserLoc().getLon();
        paramV2.userLoc.lat = searchRequestInfo.getUserLoc().getLat();
        paramV2.poiLoc.lon = searchRequestInfo.getPoiLoc().getLon();
        paramV2.poiLoc.lat = searchRequestInfo.getPoiLoc().getLat();
        paramV2.geoObj = searchRequestInfo.getGeoobj();
        paramV2.city = String.valueOf(searchRequestInfo.getAdCode());
        paramV2.range = "5000";
        return paramV2;
    }

    /**
     * 能耗:到达时间，距离，到达剩余电量
     *
     * @param requestParameter SearchRequestParameter
     * @return GNavigationEtaqueryRequestParam
     */
    public GNavigationEtaqueryRequestParam convertToGNavigationEtaqueryRequestParam(final SearchRequestParameter requestParameter) {
        final GNavigationEtaqueryRequestParam javaRequest = new GNavigationEtaqueryRequestParam();
        javaRequest.route.option = "0x01";
        javaRequest.route.strategy = 1;
        javaRequest.OneToN = "0";
        javaRequest.vehicle.type = BevPowerCarUtils.getInstance().carType;
        javaRequest.vehicle.size = "0";
        javaRequest.vehicle.height = "2.80";
        javaRequest.vehicle.load = "45.0";
        javaRequest.vehicle.width = DEFAULT_VALUE;
        javaRequest.vehicle.weight = DEFAULT_VALUE;
        javaRequest.vehicle.axis = DEFAULT_VALUE;
        javaRequest.vehicle.plate = "";
        javaRequest.vehicle.elec.orga = "jaguar";
        javaRequest.vehicle.elec.drive_train = 6;
        javaRequest.vehicle.elec.fes_mode = 0;
//        javaRequest.vehicle.elec.cost_model_switch = 63;
        javaRequest.vehicle.elec.top_speed = 150;
        javaRequest.vehicle.elec.mass = 1440;
        javaRequest.vehicle.elec.charge = BevPowerCarUtils.getInstance().initlialHVBattenergy;
        javaRequest.vehicle.elec.cost_uint = 1;
        javaRequest.vehicle.elec.has_traffic = 1;
        javaRequest.vehicle.elec.vehicleConfiguration = "A6L";//使用自学习模型时必须填入
        final GNavigationEtaqueryReqElecConstList stConstList = new GNavigationEtaqueryReqElecConstList();
        stConstList.id = 0;
        stConstList.type = 0;
        stConstList.aux = 0.12;
        stConstList.ferry_rate = 0.24;

        final GNavigationEtaqueryReqElecConstListRangeEnergy stEnergy = new GNavigationEtaqueryReqElecConstListRangeEnergy();
        stEnergy.value = "4";
        stConstList.range.energy.add(stEnergy);

        final GNavigationEtaqueryReqElecConstListRangeEnergy stCostEnergy = new GNavigationEtaqueryReqElecConstListRangeEnergy();
        stCostEnergy.speed = "7";
        stCostEnergy.value = "23.5";
        stConstList.speed.cost.add(stCostEnergy);
        stConstList.trans.access = 4500000.7;
        stConstList.trans.decess = 7200000.9;
        stConstList.curve.access = 4500000.8;
        stConstList.curve.decess = 7200000.9;
        stConstList.slope.up = "4500000";
        stConstList.slope.down = "7200000";
        javaRequest.vehicle.elec.cost_list.add(stConstList);

        final GNavigationEtaqueryReqStartPoints stStartPointData = new GNavigationEtaqueryReqStartPoints();
        stStartPointData.idx = 13;
        stStartPointData.type = 2;
        stStartPointData.lon = requestParameter.getUserLoc().getLon();
        stStartPointData.lat = requestParameter.getUserLoc().getLat();
        javaRequest.start.points.add(stStartPointData);

        final GNavigationEtaqueryReqStartPoints stEndPointData = new GNavigationEtaqueryReqStartPoints();
        stEndPointData.idx = 143;
        stEndPointData.type = 2;
        stEndPointData.lon = requestParameter.getPoiLoc().getLon();
        stEndPointData.lat = requestParameter.getPoiLoc().getLat();
        javaRequest.end.points.add(stEndPointData);
        return javaRequest;
    }

    /**
     * 将经纬度列表拼接成字符串
     * @param pointList 经纬度列表
     * @param flag 分隔符
     * @return  拼接字符串
     */
    private String convertPointList2String(@Nullable final List<GeoPoint> pointList, @Nullable final String flag) {
        final int size = pointList == null ? 0 : pointList.size();
        String flag1 = flag;
        if (size == 0) {
            return "";
        }
        if (null == flag || flag.isEmpty()) {
            flag1 = ";";
        }

        final StringBuilder sb = new StringBuilder(100);
        for (int i = 0; i < size; i++) {
            final GeoPoint geoPoint = pointList.get(i);
            if (geoPoint == null) {
                continue;
            }

            sb.append(geoPoint.getLon()).append(flag1).append(geoPoint.getLat());
            if (i != size - 1) {
                sb.append(flag1);
            }
        }
        return sb.toString();
    }
}
