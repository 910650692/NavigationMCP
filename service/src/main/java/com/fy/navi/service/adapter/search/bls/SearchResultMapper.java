package com.fy.navi.service.adapter.search.bls;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_SERVICE_TAG;

import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.search.model.AggregateSearchResult;
import com.autonavi.gbl.search.model.AlongWayPoi;
import com.autonavi.gbl.search.model.ChargingPlugInfo;
import com.autonavi.gbl.search.model.KeywordSearchResultV2;
import com.autonavi.gbl.search.model.LineDeepQueryType;
import com.autonavi.gbl.search.model.LinePoiChargeInfo;
import com.autonavi.gbl.search.model.LinePoiGasStationInfo;
import com.autonavi.gbl.search.model.LinePoiParkRecommendInfo;
import com.autonavi.gbl.search.model.LinePoiServiceAreaChild;
import com.autonavi.gbl.search.model.LinePoiServiceAreaInfo;
import com.autonavi.gbl.search.model.NearestPoi;
import com.autonavi.gbl.search.model.PoiDetailProductInfo;
import com.autonavi.gbl.search.model.PoiDetailSearchResult;
import com.autonavi.gbl.search.model.PoiDetailShelfInfo;
import com.autonavi.gbl.search.model.SearchAlongWayResult;
import com.autonavi.gbl.search.model.SearchCategoryInfo;
import com.autonavi.gbl.search.model.SearchChildCategoryInfo;
import com.autonavi.gbl.search.model.SearchDeepInfoResult;
import com.autonavi.gbl.search.model.SearchEnroutePoiInfo;
import com.autonavi.gbl.search.model.SearchEnrouteResult;
import com.autonavi.gbl.search.model.SearchLineDeepInfoResult;
import com.autonavi.gbl.search.model.SearchNearestResult;
import com.autonavi.gbl.search.model.SearchPoiChildInfo;
import com.autonavi.gbl.search.model.SearchPoiInfo;
import com.autonavi.gbl.search.model.SearchSuggestionPoiTip;
import com.autonavi.gbl.search.model.SearchTipsCityInfo;
import com.autonavi.gbl.search.model.SuggestionSearchResult;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.ChildInfo;
import com.fy.navi.service.define.search.CityInfo;
import com.fy.navi.service.define.search.GasStationInfo;
import com.fy.navi.service.define.search.ParkingInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchCategoryLocalInfo;
import com.fy.navi.service.define.search.SearchChildCategoryLocalInfo;
import com.fy.navi.service.define.search.SearchParkInOutInfo;
import com.fy.navi.service.define.search.SearchRequestParameter;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.define.search.SearchRetainParamInfo;
import com.fy.navi.service.define.search.SearchTipsCityLocalInfo;
import com.fy.navi.service.define.search.SearchTipsLocalInfo;
import com.fy.navi.service.define.search.ServiceAreaInfo;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * SearchResultMapper类负责将搜索结果映射到特定的数据结构。
 */
public class SearchResultMapper {

    private static class Holder {
        private static final SearchResultMapper INSTANCE = new SearchResultMapper();
    }

    private SearchResultMapper() {
    }

    public static SearchResultMapper getInstance() {
        return Holder.INSTANCE;
    }

    /**
     * 将预搜索结果 SuggestionSearchResult 转换为 SearchResultEntity
     */
    public SearchResultEntity mapFromSuggestionSearchResult(SearchRequestParameter requestParameterBuilder, SuggestionSearchResult result) {
        SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, result.code, result.message);
        List<PoiInfoEntity> poiList = Optional.ofNullable(result.tipList)
                .orElse(new ArrayList<>())
                .stream()
                .map(this::mapSuggestionPoiTip)
                .collect(Collectors.toList());
        searchResultEntity.setPoiList(poiList);
        return searchResultEntity;
    }

    /**
     * 映射 SearchSuggestionPoiTip 到 PoiInfoEntity
     */
    private PoiInfoEntity mapSuggestionPoiTip(SearchSuggestionPoiTip suggestionPoiTip) {
        GeoPoint point = new GeoPoint();
        point.setLat(suggestionPoiTip.basicInfo.location.lat);
        point.setLon(suggestionPoiTip.basicInfo.location.lon);
        return new PoiInfoEntity()
                .setPid(suggestionPoiTip.basicInfo.poiId)
                .setName(suggestionPoiTip.basicInfo.name)
                .setAddress(suggestionPoiTip.basicInfo.address)
                .setDistance(formatDistanceArrayInternal(ConvertUtils.double2int(suggestionPoiTip.basicInfo.distance)))
                .setSort_distance(ConvertUtils.double2int(suggestionPoiTip.basicInfo.distance))
                .setPoint(point);
    }

    /**
     * 将关键字搜索结果 KeywordSearchResultV2 转换为 SearchResultEntity
     */
    public SearchResultEntity mapFromKeywordSearchResultV2(SearchRequestParameter requestParameterBuilder, KeywordSearchResultV2 result) {
        SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, result.code, result.message);
        List<PoiInfoEntity> poiList = Optional.ofNullable(result.poiList)
                .orElse(new ArrayList<>())
                .stream()
                .map(this::mapSearchPoiInfo)
                .collect(Collectors.toList());
        searchResultEntity.setMaxPageNum(result.total / 10 + 1);
        searchResultEntity.setTotal(result.total);
        searchResultEntity.setPoiList(poiList);
        searchResultEntity.setPoiType(result.poiType);//0=离线数据，1=在线数据
        //获取筛选分类数据
        if (result.classify != null) {
            searchResultEntity.setRetain(result.classify.retainState);//筛选搜索需要的信息
            Logger.d(SEARCH_SERVICE_TAG, "mapFromKeywordSearchResultV2 retainState: " + result.classify.retainState);
            if (result.classify.classifyItemInfo != null) {
                List<SearchCategoryLocalInfo> categoryLocalInfoList = Optional.ofNullable(result.classify.classifyItemInfo.categoryInfoList)
                        .orElse(new ArrayList<>())
                        .stream()
                        .map(this::mapSearchCategoryInfo)
                        .collect(Collectors.toList());
                if (!categoryLocalInfoList.isEmpty()) {
                    for (int i = 0; i < categoryLocalInfoList.size(); i++) {
                        List<SearchChildCategoryLocalInfo> level1Infos = Optional.ofNullable(result.classify.classifyItemInfo.categoryInfoList.
                                        get(i).childCategoryInfo)
                                .orElse(new ArrayList<>())
                                .stream()
                                .map(this::mapSearchChildCategoryInfo)
//                                .filter(this::shouldIncludeInResult)//过滤value或者name值不存在的item
                                .collect(Collectors.toList());
                        categoryLocalInfoList.get(i).setCategoryLocalInfos(level1Infos);
                        if (!level1Infos.isEmpty()) {
                            for (int j = 0; j < level1Infos.size(); j++) {
                                List<SearchChildCategoryLocalInfo> level1ChildInfos = Optional.ofNullable(result.classify.classifyItemInfo.categoryInfoList.
                                                get(i).childCategoryInfo.
                                                get(j).childCategoryInfoList)
                                        .orElse(new ArrayList<>())
                                        .stream()
                                        .map(this::mapSearchChildCategoryInfo)
//                                        .filter(this::shouldIncludeInResult)//过滤value或者name值不存在的item
                                        .collect(Collectors.toList());
                                level1Infos.get(j).setCategoryLocalInfos(level1ChildInfos);
                            }
                        }
                    }
                }
                //测试log，测试时放开
//                for (SearchCategoryLocalInfo searchCategoryLocalInfo : categoryLocalInfoList) {
//                    Logger.d(SEARCH_SERVICE_TAG, "mapFromKeywordSearchResultV2 name: " + searchCategoryLocalInfo.getName()
//                            + " checked: " + searchCategoryLocalInfo.getChecked());
//                    for (SearchChildCategoryLocalInfo searchChildCategoryLocalInfo : searchCategoryLocalInfo.getCategoryLocalInfos()) {
//                        Logger.d(SEARCH_SERVICE_TAG, "mapFromKeywordSearchResultV2 level name: " + searchChildCategoryLocalInfo.getName()
//                                + " checked: " + searchChildCategoryLocalInfo.getChecked()
//                                + " value: " + searchChildCategoryLocalInfo.getValue());
//                        for (SearchChildCategoryLocalInfo searchChildCategoryLocalInfo1 : searchChildCategoryLocalInfo.getCategoryLocalInfos()) {
//                            Logger.d(SEARCH_SERVICE_TAG, "mapFromKeywordSearchResultV2 level child name: " + searchChildCategoryLocalInfo1.getName()
//                                    + " checked: " + searchChildCategoryLocalInfo1.getChecked()
//                                    + " value: " + searchChildCategoryLocalInfo1.getValue());
//                        }
//                    }
//                }
                searchResultEntity.setLocalInfoList(categoryLocalInfoList);
            }
        }

        //回传参数，POI详情搜索必传，来自于关键字搜索结果,存储后用于之后POI详情搜索
        if (result.lqii != null && result.lqii.tipsInfo != null) {
            List<SearchTipsCityLocalInfo> cityLocalInfoList = Optional.ofNullable(result.lqii.tipsInfo.optionalCityList)
                    .orElse(new ArrayList<>())
                    .stream()
                    .map(this::mapSearchTipsCityInfo)
                    .collect(Collectors.toList());
            SearchTipsLocalInfo searchTipsLocalInfo = new SearchTipsLocalInfo()
                    .setSceneType(result.lqii.tipsInfo.sceneType)
                    .setCityName(result.lqii.tipsInfo.cityName)
                    .setKeyword(result.lqii.tipsInfo.keyword)
                    .setOptionalKeywordList(result.lqii.tipsInfo.optionalKeywordList)
                    .setRetainState(result.lqii.tipsInfo.retainState)
                    .setOptionalCityList(cityLocalInfoList);

            SearchRetainParamInfo searchRetainParamInfo = new SearchRetainParamInfo()
                    .setKeywordBizType(result.retainParam.keywordBizType)
                    .setSearchTipsLocalInfo(searchTipsLocalInfo);
            for (PoiInfoEntity infoEntity : poiList) {
                infoEntity.setRetainParam(searchRetainParamInfo);
            }
        }
        return searchResultEntity;
    }

    private boolean shouldIncludeInResult(SearchChildCategoryLocalInfo localInfo) {
        return !ConvertUtils.isEmpty(localInfo.getName()) && !ConvertUtils.isEmpty(localInfo.getValue());
    }

    /**
     * 映射 SearchCategoryInfo 到 SearchCategoryLocalInfo
     */
    private SearchCategoryLocalInfo mapSearchCategoryInfo(SearchCategoryInfo searchCategoryInfo) {
        return new SearchCategoryLocalInfo()
                .setName(searchCategoryInfo.baseInfo.name)
                .setChecked(searchCategoryInfo.baseInfo.checked);
    }

    /**
     * 映射 SearchChildCategoryInfo 到 SearchChildCategoryLocalInfo
     */
    private SearchChildCategoryLocalInfo mapSearchChildCategoryInfo(SearchChildCategoryInfo searchChildCategoryInfo) {
        return new SearchChildCategoryLocalInfo()
                .setName(searchChildCategoryInfo.baseInfo.name)
                .setValue(searchChildCategoryInfo.baseInfo.value)
                .setChecked(searchChildCategoryInfo.baseInfo.checked);
    }

    /**
     * 映射 SearchTipsCityInfo 到 SearchTipsCityLocalInfo
     */
    private SearchTipsCityLocalInfo mapSearchTipsCityInfo(SearchTipsCityInfo searchTipsCityInfo) {
        return new SearchTipsCityLocalInfo()
                .setAdcode(searchTipsCityInfo.adcode)
                .setIndex(searchTipsCityInfo.index)
                .setName(searchTipsCityInfo.name);
    }

    /**
     * 映射 SearchPoiChildInfo 到 ChildInfo
     */
    private ChildInfo mapSearchPoiChildInfo(SearchPoiChildInfo searchPoiChildInfo) {
        return new ChildInfo()
                .setName(searchPoiChildInfo.name)
                .setShortName(searchPoiChildInfo.shortName)
                .setRatio(searchPoiChildInfo.ratio)
                .setLabel(searchPoiChildInfo.label)
                .setPoiId(searchPoiChildInfo.poiId)
                .setLocation(new GeoPoint(searchPoiChildInfo.location.lon, searchPoiChildInfo.location.lat))
                .setAddress(searchPoiChildInfo.address);
    }

    /**
     * 映射 SearchPoiInfo 到 PoiInfoEntity
     */
    private PoiInfoEntity mapSearchPoiInfo(SearchPoiInfo searchPoiInfo) {
        GeoPoint point = new GeoPoint();
        point.setLat(searchPoiInfo.basicInfo.location.lat);
        point.setLon(searchPoiInfo.basicInfo.location.lon);
        //加油站信息
        List<GasStationInfo> gasStationInfos = new ArrayList<>();
        for (int i = 0; i < searchPoiInfo.gasInfo.typeList.size(); i++) {
            gasStationInfos.add(new GasStationInfo()
                    .setType(searchPoiInfo.gasInfo.typeList.get(i))
                    .setPrice(searchPoiInfo.gasInfo.priceList.get(i)));
            Logger.d(SEARCH_SERVICE_TAG, "gas type is: " + searchPoiInfo.gasInfo.typeList.get(i)
                    + "gas price is: " + searchPoiInfo.gasInfo.priceList.get(i));
        }
        //充电站信息
        List<ChargeInfo> chargeInfos = new ArrayList<>();
        int slowFree = TextUtils.isEmpty(searchPoiInfo.chargingStationInfo.slow_free) ? 0 : Integer.parseInt(searchPoiInfo.chargingStationInfo.slow_free);
        int slowTotal = TextUtils.isEmpty(searchPoiInfo.chargingStationInfo.slow_total) ? 0 : Integer.parseInt(searchPoiInfo.chargingStationInfo.slow_total);
        int fastFree = TextUtils.isEmpty(searchPoiInfo.chargingStationInfo.fast_free) ? 0 : Integer.parseInt(searchPoiInfo.chargingStationInfo.fast_free);
        int fastTotal = TextUtils.isEmpty(searchPoiInfo.chargingStationInfo.fast_total) ? 0 : Integer.parseInt(searchPoiInfo.chargingStationInfo.fast_total);
        ChargeInfo chargeInfo = new ChargeInfo()
                .setSlow_free(slowFree)
                .setSlow_total(slowTotal)
                .setFast_free(fastFree)
                .setFast_total(fastTotal)
                .setCurrentElePrice(searchPoiInfo.chargingStationInfo.current_ele_price)
                .setCurrentServicePrice(searchPoiInfo.chargingStationInfo.parkPrice);
        Logger.d(SEARCH_SERVICE_TAG, "slow free: " + searchPoiInfo.chargingStationInfo.slow_free
                + " slow total: " + searchPoiInfo.chargingStationInfo.slow_total
                + " fast free: " + searchPoiInfo.chargingStationInfo.fast_free
                + " fast total: " + searchPoiInfo.chargingStationInfo.fast_total
                + " current_ele_price: " + searchPoiInfo.chargingStationInfo.current_ele_price
                + " parkPrice: " + searchPoiInfo.chargingStationInfo.parkPrice);
        for (ChargingPlugInfo chargingPlugInfo : searchPoiInfo.chargingStationInfo.plugsInfo) {
            if (chargingPlugInfo.plugType == AutoMapConstant.PLUG_TYPE_SLOW) {
                chargeInfo.setSlowVolt(chargingPlugInfo.slowVoltage)
                        .setSlowPower(chargingPlugInfo.slowPower);
            }
            if (chargingPlugInfo.plugType == AutoMapConstant.PLUG_TYPE_FAST) {
                chargeInfo.setFastVolt(chargingPlugInfo.fastVoltage)
                        .setFastPower(chargingPlugInfo.fastPower);
            }
            Logger.d(SEARCH_SERVICE_TAG, "fast volt: " + chargeInfo.getFastVolt()
                    + " fast power: " + chargeInfo.getFastPower()
                    + " slow volt: " + chargeInfo.getSlowVolt()
                    + " slow power: " + chargeInfo.getSlowPower()
                    + " plugType: " + chargingPlugInfo.plugType);
        }
        chargeInfos.add(chargeInfo);

        //子节点信息
        List<ChildInfo> childInfoList = Optional.ofNullable(searchPoiInfo.childInfoList)
                .orElse(new ArrayList<>())
                .stream()
                .map(this::mapSearchPoiChildInfo)
                .collect(Collectors.toList());
        for (ChildInfo childInfo : childInfoList) {
            Logger.d(SEARCH_SERVICE_TAG, "childInfo.name is: " + childInfo.name +
                    " childInfo.poiId is: " + childInfo.poiId + " childInfo.address is: "
                    + childInfo.address + " childInfo.ratio is: " + childInfo.ratio
                    + " childInfo.shortName is: " + childInfo.shortName);
        }
        //停车场信息
        List<ParkingInfo> parkingInfoList = new ArrayList<>();
        // 停车场出入口信息
        List<SearchParkInOutInfo> searchParkInOutInfos = searchPoiInfo.parkingInfo.inoutInfoList
                .stream()
                .map(inoutInfo -> new SearchParkInOutInfo()
                        .setEntExitId(inoutInfo.entExitId)
                        .setKeytype(inoutInfo.keytype)
                        .setX(inoutInfo.x)
                        .setY(inoutInfo.y))
                .collect(Collectors.toList());

        ParkingInfo parkingInfo = new ParkingInfo()
                .setSpaceTotal(searchPoiInfo.parkingInfo.dynamicParking.spaceTotal)
                .setSpaceFree(searchPoiInfo.parkingInfo.dynamicParking.spaceFree)
                .setBusyStatus(searchPoiInfo.parkingInfo.dynamicParking.busyStatus)
                .setSrcType(searchPoiInfo.parkingInfo.dynamicParking.srcType)
                .setSpace(searchPoiInfo.parkingInfo.space)
                .setFee(searchPoiInfo.parkingInfo.fee)
                .setGeometry(searchPoiInfo.parkingInfo.geometry)
                .setDayCharge(searchPoiInfo.parkingInfo.dayCharge)
                .setNightCharge(searchPoiInfo.parkingInfo.nightCharge)
                .setCategory(searchPoiInfo.parkingInfo.category)
                .setParkingSrcType(searchPoiInfo.parkingInfo.parkingSrcType)
                .setSearchParkInOutInfos(searchParkInOutInfos);
        parkingInfoList.add(parkingInfo);
        Logger.e(SEARCH_SERVICE_TAG, "typeCode is: " + searchPoiInfo.basicInfo.typeCode
                + " ;name is: " + searchPoiInfo.basicInfo.name
                + "  ;searchPoiInfo.basicInfo.distance:" + searchPoiInfo.basicInfo.distance
                + " ;searchPoiInfo.basicInfo.pid:" + searchPoiInfo.basicInfo.poiId);
        return new PoiInfoEntity()
                .setPointTypeCode(searchPoiInfo.basicInfo.typeCode)
                .setPid(searchPoiInfo.basicInfo.poiId)
                .setName(searchPoiInfo.basicInfo.name)
                .setAddress(searchPoiInfo.basicInfo.address)
                .setDistance(formatDistanceArrayInternal(ConvertUtils.double2int(ConvertUtils.str2Double(searchPoiInfo.basicInfo.distance))))

                .setPoint(point)
                .setAdCode(searchPoiInfo.basicInfo.adcode)
                .setIndustry(searchPoiInfo.basicInfo.industry)
                .setPhone(searchPoiInfo.basicInfo.tel)
                .setImageUrl(searchPoiInfo.basicInfo.imageUrl)
                .setBusinessTime(searchPoiInfo.basicInfo.openTime)
                .setRating(searchPoiInfo.basicInfo.rating)
                .setAverageCost(searchPoiInfo.basicInfo.averageCost)
                .setPoiTag(isParking(searchPoiInfo.basicInfo.typeCode) ? "停车场" : searchPoiInfo.basicInfo.tag)
                .setParkingInfoList(parkingInfoList)
                .setChildInfoList(childInfoList)
                .setStationList(gasStationInfos)
                .setSort_distance(ConvertUtils.str2Int(searchPoiInfo.basicInfo.distance))
                .setSort_rate(ConvertUtils.str2Int(searchPoiInfo.rankInfo.rankNo))
                .setSort_price(ConvertUtils.str2Int(searchPoiInfo.hotelInfo.priceLowest))
                .setChargeInfoList(chargeInfos);
    }

    /**
     * 将Poi id搜索结果 KeywordSearchResultV2 转换为 SearchResultEntity
     */
    public SearchResultEntity mapFromPoiDetailsSearchResult(SearchRequestParameter requestParameterBuilder, KeywordSearchResultV2 result) {
        SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, result.code, result.message);
        List<PoiInfoEntity> poiList = Optional.ofNullable(result.poiList)
                .orElse(new ArrayList<>())
                .stream()
                .map(this::mapSearchPoiInfo)
                .collect(Collectors.toList());
        searchResultEntity.setPoiList(poiList);
        searchResultEntity.setPoiType(result.poiType);
        return searchResultEntity;
    }

    /**
     * 将Geo 搜索结果 SearchNearestResult 转换为 SearchResultEntity
     */
    public SearchResultEntity mapFromGeoSearchResult(SearchRequestParameter requestParameterBuilder, SearchNearestResult result) {
        SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, result.code, result.message);

        if (result.poi_list == null || result.poi_list.isEmpty()) {
            Logger.e(SEARCH_SERVICE_TAG, "GeoSearchResult poi_list is empty.");
            searchResultEntity.setPoiList(Collections.emptyList());
            searchResultEntity.setSearchType(AutoMapConstant.SearchType.GEO_SEARCH);
            return searchResultEntity;
        }
        searchResultEntity.setPoiType(result.iPoiType);
        // 获取第一个 POI 信息
        NearestPoi poiItem = result.poi_list.get(0);
        GeoPoint poiPoint = new GeoPoint(poiItem.point.lon, poiItem.point.lat);

        // 构建 CityInfo
        CityInfo cityInfo = buildCityInfo(result, poiPoint);

        // 构建 PoiInfoEntity
        PoiInfoEntity poiInfoEntity = buildPoiInfoEntity(poiItem, poiPoint, cityInfo);

        // 设置 POI 列表并返回结果
        searchResultEntity.setPoiList(Collections.singletonList(poiInfoEntity));
        return searchResultEntity;
    }

    private CityInfo buildCityInfo(SearchNearestResult result, GeoPoint poiPoint) {
        return new CityInfo()
                .setCityName(result.city)
                .setCityCode(result.cityadcode)
                .setProvince(result.province)
                .setProvinceAdCode(result.provinceadcode)
                .setAreaCode(result.areacode)
                .setDistrict(result.district)
                .setDistrictAdCode(result.districtadcode)
                .setAdCode(result.adcode)
                .setPos(result.pos)
                .setAddress(result.pos)
                .setCityPoint(poiPoint);
    }

    private PoiInfoEntity buildPoiInfoEntity(NearestPoi poiItem, GeoPoint poiPoint, CityInfo cityInfo) {
        return new PoiInfoEntity()
                .setPointTypeCode(poiItem.typecode)
                .setPid(poiItem.poiid)
                .setName(poiItem.name)
                .setAddress(poiItem.address)
                .setDistance(formatDistanceArrayInternal(poiItem.distance))
                .setSort_price(poiItem.distance)
                .setPoint(poiPoint)
                .setAdCode(poiItem.nAdCode)
                .setPhone(poiItem.tel)
                .setBusinessTime("")
                .setCityInfo(cityInfo);
    }


    /**
     * 将沿途搜索结果 SearchAlongWayResult 转换为 SearchResultEntity
     */
    public SearchResultEntity mapFromAlongWayResult(SearchRequestParameter requestParameterBuilder, SearchAlongWayResult result) {
        SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, result.code, result.message);
        List<PoiInfoEntity> poiList = Optional.ofNullable(result.pois)
                .orElse(new ArrayList<>())
                .stream()
                .map(this::mapSearchPoiInfo)
                .collect(Collectors.toList());
        searchResultEntity.setPoiList(poiList);
        return searchResultEntity;
    }


    private PoiInfoEntity mapSearchPoiInfo(AlongWayPoi nearestPoi) {
        GeoPoint point = new GeoPoint();
        point.setLat(nearestPoi.point.lat);
        point.setLon(nearestPoi.point.lon);
        return new PoiInfoEntity()
                .setPid(nearestPoi.id)
                .setName(nearestPoi.name)
                .setAddress(nearestPoi.address)
                .setDistance(formatDistanceArrayInternal(ConvertUtils.double2int(ConvertUtils.str2Double(nearestPoi.distance))))
                .setSort_distance(ConvertUtils.double2int(ConvertUtils.str2Double(nearestPoi.distance)))
                .setPoint(point)
                .setAdCode(nearestPoi.nAdCode);
    }

    /**
     * 将聚合搜索 AggregateSearchResult 转换为 SearchResultEntity
     */
    public SearchResultEntity mapFromAggregateSearchResult(SearchRequestParameter requestParameterBuilder, AggregateSearchResult result) {
        SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, result.code, result.message);
        List<PoiInfoEntity> poiList = new ArrayList<>();
        switch (requestParameterBuilder.getType()) {
            case AutoMapConstant.AggregateKeywordType.DINING:
                if (result.dinings != null) {
                    result.dinings.stream()
                            .filter(aggregateDiningResult -> aggregateDiningResult.poiInfo != null)
                            .forEach(aggregateDiningResult -> {
                                try {
                                    int distance = ConvertUtils.str2Int(aggregateDiningResult.poiInfo.distance);
                                    PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                                            .setPid(aggregateDiningResult.poiInfo.poiId)
                                            .setName(aggregateDiningResult.poiInfo.name)
                                            .setAddress(aggregateDiningResult.poiInfo.address)
                                            .setDistance(formatDistanceArrayInternal(distance))
                                            .setSort_distance(distance)
                                            .setPoint(new GeoPoint(aggregateDiningResult.poiInfo.location.lon, aggregateDiningResult.poiInfo.location.lat))
                                            .setAdCode(aggregateDiningResult.poiInfo.adcode);
                                    poiList.add(poiInfoEntity);
                                } catch (NumberFormatException e) {
                                    Logger.d(SEARCH_SERVICE_TAG, "NumberFormatException :" + e.getMessage());
                                }
                            });
                } else {
                    Logger.d(SEARCH_SERVICE_TAG, "parkings is null");
                }
                break;
            case AutoMapConstant.AggregateKeywordType.SCENIC:
                if (result.scenics != null) {
                    result.scenics.stream()
                            .filter(aggregateDiningResult -> aggregateDiningResult.poiInfo != null)
                            .forEach(aggregateDiningResult -> {
                                try {
                                    int distance = ConvertUtils.str2Int(aggregateDiningResult.poiInfo.distance);
                                    PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                                            .setPid(aggregateDiningResult.poiInfo.poiId)
                                            .setName(aggregateDiningResult.poiInfo.name)
                                            .setAddress(aggregateDiningResult.poiInfo.address)
                                            .setDistance(formatDistanceArrayInternal(distance))
                                            .setSort_distance(distance)
                                            .setPoint(new GeoPoint(aggregateDiningResult.poiInfo.location.lon, aggregateDiningResult.poiInfo.location.lat))
                                            .setAdCode(aggregateDiningResult.poiInfo.adcode);
                                    poiList.add(poiInfoEntity);
                                } catch (NumberFormatException e) {
                                    Logger.d(SEARCH_SERVICE_TAG, "NumberFormatException :" + e.getMessage());
                                }
                            });
                } else {
                    Logger.d(SEARCH_SERVICE_TAG, "parkings is null");
                }
                break;
            case AutoMapConstant.AggregateKeywordType.MALL:
                if (result.malls != null) {
                    result.malls.stream()
                            .filter(aggregateDiningResult -> aggregateDiningResult.poiInfo != null)
                            .forEach(aggregateDiningResult -> {
                                try {
                                    int distance = ConvertUtils.str2Int(aggregateDiningResult.poiInfo.distance);
                                    PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                                            .setPid(aggregateDiningResult.poiInfo.poiId)
                                            .setName(aggregateDiningResult.poiInfo.name)
                                            .setAddress(aggregateDiningResult.poiInfo.address)
                                            .setDistance(formatDistanceArrayInternal(distance))
                                            .setSort_distance(distance)
                                            .setPoint(new GeoPoint(aggregateDiningResult.poiInfo.location.lon, aggregateDiningResult.poiInfo.location.lat))
                                            .setAdCode(aggregateDiningResult.poiInfo.adcode);
                                    poiList.add(poiInfoEntity);
                                } catch (NumberFormatException e) {
                                    Logger.d(SEARCH_SERVICE_TAG, "NumberFormatException :" + e.getMessage());
                                }
                            });
                } else {
                    Logger.d(SEARCH_SERVICE_TAG, "parkings is null");
                }
                break;
            case AutoMapConstant.AggregateKeywordType.CHARGING:
                if (result.chargings != null) {
                    result.chargings.stream()
                            .filter(aggregateDiningResult -> aggregateDiningResult.poiInfo != null)
                            .forEach(aggregateDiningResult -> {
                                try {
                                    int distance = ConvertUtils.str2Int(aggregateDiningResult.poiInfo.distance);
                                    PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                                            .setPid(aggregateDiningResult.poiInfo.poiId)
                                            .setName(aggregateDiningResult.poiInfo.name)
                                            .setAddress(aggregateDiningResult.poiInfo.address)
                                            .setDistance(formatDistanceArrayInternal(distance))
                                            .setSort_distance(distance)
                                            .setPoint(new GeoPoint(aggregateDiningResult.poiInfo.location.lon, aggregateDiningResult.poiInfo.location.lat))
                                            .setAdCode(aggregateDiningResult.poiInfo.adcode);
                                    poiList.add(poiInfoEntity);
                                } catch (NumberFormatException e) {
                                    Logger.d(SEARCH_SERVICE_TAG, "NumberFormatException :" + e.getMessage());
                                }
                            });
                } else {
                    Logger.d(SEARCH_SERVICE_TAG, "parkings is null");
                }
                break;
            case AutoMapConstant.AggregateKeywordType.PARKING:
                if (result.parkings != null) {
                    result.parkings.stream()
                            .filter(aggregateDiningResult -> aggregateDiningResult.poiInfo != null)
                            .forEach(aggregateDiningResult -> {
                                try {
                                    int distance = ConvertUtils.str2Int(aggregateDiningResult.poiInfo.distance);
                                    PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                                            .setPid(aggregateDiningResult.poiInfo.poiId)
                                            .setName(aggregateDiningResult.poiInfo.name)
                                            .setAddress(aggregateDiningResult.poiInfo.address)
                                            .setDistance(formatDistanceArrayInternal(distance))
                                            .setSort_distance(distance)
                                            .setPoint(new GeoPoint(aggregateDiningResult.poiInfo.location.lon, aggregateDiningResult.poiInfo.location.lat))
                                            .setAdCode(aggregateDiningResult.poiInfo.adcode);
                                    poiList.add(poiInfoEntity);
                                } catch (NumberFormatException e) {
                                    Logger.d(SEARCH_SERVICE_TAG, "NumberFormatException :" + e.getMessage());
                                }
                            });
                } else {
                    Logger.d(SEARCH_SERVICE_TAG, "parkings is null");
                }
                break;
            case AutoMapConstant.AggregateKeywordType.BATHROOM:
                if (result.others != null) {
                    result.others.stream()
                            .filter(aggregateDiningResult -> aggregateDiningResult.bathroom != null)
                            .forEach(aggregateDiningResult -> {
                                try {
                                    int distance = ConvertUtils.str2Int(aggregateDiningResult.bathroom.distance);
                                    PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                                            .setPid(aggregateDiningResult.bathroom.poiId)
                                            .setName(aggregateDiningResult.bathroom.name)
                                            .setAddress(aggregateDiningResult.bathroom.address)
                                            .setDistance(formatDistanceArrayInternal(distance))
                                            .setSort_distance(distance)
                                            .setPoint(new GeoPoint(aggregateDiningResult.bathroom.location.lon, aggregateDiningResult.bathroom.location.lat))
                                            .setAdCode(aggregateDiningResult.bathroom.adcode);
                                    poiList.add(poiInfoEntity);
                                } catch (NumberFormatException e) {
                                    Logger.d(SEARCH_SERVICE_TAG, "NumberFormatException :" + e.getMessage());
                                }
                            });
                } else {
                    Logger.d(SEARCH_SERVICE_TAG, "others is null");
                }
                break;
            case AutoMapConstant.AggregateKeywordType.UNKNOWN:
                Logger.d(SEARCH_SERVICE_TAG, "AggregateKeywordType.UNKNOWN");
                break;
        }
        searchResultEntity.setPoiList(poiList);
        return searchResultEntity;
    }

    /**
     * 将顺路关键字搜索结果 SearchEnRouteResult 转换为 SearchResultEntity
     */
    public SearchResultEntity mapFromSearchEnRouteResult(SearchRequestParameter requestParameterBuilder, SearchEnrouteResult result) {
        SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, result.errorCode, result.errorMessage);
        List<PoiInfoEntity> poiList = Optional.ofNullable(result.poiInfos)
                .orElse(new ArrayList<>())
                .stream()
                .map(this::convertToPoiInfoEntity)
                .sorted(Comparator.comparingInt(PoiInfoEntity::getSort_distance))
                .collect(Collectors.toList());
        searchResultEntity.setPoiList(poiList);
        searchResultEntity.setPoiType(1);//0=离线数据，1=在线数据
        return searchResultEntity;
    }

    /**
     * 将POI详情搜索结果 PoiDetailSearchResult 转换为 SearchResultEntity
     */
    public SearchResultEntity mapFromSearchPoiDetailSearchResult(SearchRequestParameter requestParameterBuilder, PoiDetailSearchResult result) {
        SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, 0, "");
        Logger.d(SEARCH_SERVICE_TAG, "name :" + result.baseInfo.poiInfo.poiInfoBase.name
                + "address :" + result.baseInfo.poiInfo.poiInfoBase.address);
        for (PoiDetailShelfInfo buyInfoList : result.groupBuyInfoList) {
            for (PoiDetailProductInfo productInfo : buyInfoList.productInfoList) {
                Logger.d(SEARCH_SERVICE_TAG, "productName :" + productInfo.spuName
                        + "productCurrentPrice :" + productInfo.currentPrice
                        + "productOriginalPrice :" + productInfo.originalPrice);
            }
        }
        return searchResultEntity;
    }

    private PoiInfoEntity convertToPoiInfoEntity(SearchEnroutePoiInfo poiInfo) {
        //加油站信息
        List<GasStationInfo> gasStationInfos = new ArrayList<>();
        for (int i = 0; i < poiInfo.gasStationInfo.types.size(); i++) {
            gasStationInfos.add(new GasStationInfo()
                    .setType(poiInfo.gasStationInfo.types.get(i))
                    .setPrice(poiInfo.gasStationInfo.prices.get(i)));
            Logger.d(SEARCH_SERVICE_TAG, "gas type is: " + poiInfo.gasStationInfo.types.get(i)
                    + "gas price is: " + poiInfo.gasStationInfo.prices.get(i));
        }
        //充电站信息
        List<ChargeInfo> chargeInfos = new ArrayList<>();
        int slowFree = TextUtils.isEmpty(poiInfo.chargingStationInfo.slow_free) ? 0 : Integer.parseInt(poiInfo.chargingStationInfo.slow_free);
        int slowTotal = TextUtils.isEmpty(poiInfo.chargingStationInfo.slow_total) ? 0 : Integer.parseInt(poiInfo.chargingStationInfo.slow_total);
        int fastFree = TextUtils.isEmpty(poiInfo.chargingStationInfo.fast_free) ? 0 : Integer.parseInt(poiInfo.chargingStationInfo.fast_free);
        int fastTotal = TextUtils.isEmpty(poiInfo.chargingStationInfo.fast_total) ? 0 : Integer.parseInt(poiInfo.chargingStationInfo.fast_total);
        ChargeInfo chargeInfo = new ChargeInfo()
                .setSlow_free(slowFree)
                .setSlow_total(slowTotal)
                .setFast_free(fastFree)
                .setFast_total(fastTotal)
                .setCurrentElePrice(poiInfo.chargingStationInfo.current_ele_price)
                .setCurrentServicePrice(poiInfo.chargingStationInfo.parkPrice);
        Logger.d(SEARCH_SERVICE_TAG, "slow free: " + poiInfo.chargingStationInfo.slow_free
                + " slow total: " + poiInfo.chargingStationInfo.slow_total
                + " fast free: " + poiInfo.chargingStationInfo.fast_free
                + " fast total: " + poiInfo.chargingStationInfo.fast_total
                + " current_ele_price: " + poiInfo.chargingStationInfo.current_ele_price
                + " parkPrice: " + poiInfo.chargingStationInfo.parkPrice);
        for (ChargingPlugInfo chargingPlugInfo : poiInfo.chargingStationInfo.plugsInfo) {
            if (chargingPlugInfo.plugType == AutoMapConstant.PLUG_TYPE_SLOW) {
                chargeInfo.setSlowVolt(chargingPlugInfo.slowVoltage)
                        .setSlowPower(chargingPlugInfo.slowPower);
            }
            if (chargingPlugInfo.plugType == AutoMapConstant.PLUG_TYPE_FAST) {
                chargeInfo.setFastVolt(chargingPlugInfo.fastVoltage)
                        .setFastPower(chargingPlugInfo.fastPower);
            }
            Logger.d(SEARCH_SERVICE_TAG, "fast volt: " + chargeInfo.getFastVolt()
                    + " fast power: " + chargeInfo.getFastPower()
                    + " slow volt: " + chargeInfo.getSlowVolt()
                    + " slow power: " + chargeInfo.getSlowPower()
                    + " plugType: " + chargingPlugInfo.plugType);
        }
        chargeInfos.add(chargeInfo);

        //停车场信息,对象中不包含，赋默认值
        List<ParkingInfo> parkingInfoList = new ArrayList<>();
        ParkingInfo parkingInfo = new ParkingInfo()
                .setSpaceTotal(0)
                .setSpaceFree(0);
        parkingInfoList.add(parkingInfo);
        Logger.e(SEARCH_SERVICE_TAG, "typeCode is: " + poiInfo.basicInfo.typeCode
                + " ;name is: " + poiInfo.basicInfo.name
                + "  ;searchPoiInfo.basicInfo.distance:" + poiInfo.basicInfo.distance
                + " ;searchPoiInfo.basicInfo.tag:" + poiInfo.basicInfo.tag);

        return new PoiInfoEntity()
                .setPointTypeCode(poiInfo.basicInfo.typeCode)
                .setPid(poiInfo.basicInfo.poiId)
                .setName(poiInfo.basicInfo.name)
                .setAddress(poiInfo.basicInfo.address)
                .setDistance(formatDistanceArrayInternal(ConvertUtils.str2Int(poiInfo.basicInfo.distance)))
                .setSort_distance(ConvertUtils.str2Int(poiInfo.basicInfo.distance))
                .setPoint(new GeoPoint(poiInfo.basicInfo.location.lon, poiInfo.basicInfo.location.lat))
                .setStationList(gasStationInfos)
                .setParkingInfoList(parkingInfoList)
                .setAdCode(poiInfo.basicInfo.adcode)
                .setIndustry(poiInfo.basicInfo.industry)
                .setPhone(poiInfo.basicInfo.tel)
                .setImageUrl(poiInfo.basicInfo.imageUrl)
                .setBusinessTime(poiInfo.basicInfo.openTime)
                .setRating(poiInfo.basicInfo.rating)
                .setAverageCost(poiInfo.basicInfo.averageCost)
                .setPoiTag(isParking(poiInfo.basicInfo.typeCode) ? "停车场" : poiInfo.basicInfo.tag)
                .setSort_distance(ConvertUtils.str2Int(poiInfo.basicInfo.distance))
                .setChargeInfoList(chargeInfos);
    }

    /**
     * 将深度信息 搜索结果 SearchDeepInfoResult 转换为 SearchResultEntity
     */
    public SearchResultEntity mapFromDeepInfoSearchResult(SearchRequestParameter requestParameterBuilder, SearchDeepInfoResult result) {
        SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, result.code, result.message);
        List<PoiInfoEntity> poiList = Optional.ofNullable(result.deepinfoPoi)
                .map(deepinfoPoi -> {
                    PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                            .setPid(deepinfoPoi.poiid)
                            .setName(deepinfoPoi.name)
                            .setAddress(deepinfoPoi.address)
                            .setPoint(new GeoPoint(deepinfoPoi.poi_loc.lon, deepinfoPoi.poi_loc.lat))
                            .setAdCode(deepinfoPoi.adcode)
                            .setPhone(deepinfoPoi.tel)
                            .setBusinessTime(deepinfoPoi.opentime);

                    List<GasStationInfo> stationList = Optional.ofNullable(deepinfoPoi.gasinfoList)
                            .map(gasinfoList -> gasinfoList.stream()
                                    .map(gasInfo -> new GasStationInfo()
                                            .setType(gasInfo.type)
                                            .setPrice(gasInfo.price))
                                    .collect(Collectors.toList()))
                            .orElse(Collections.emptyList());

                    poiInfoEntity.setStationList(stationList);
                    return Collections.singletonList(poiInfoEntity);
                })
                .orElse(Collections.emptyList());

        searchResultEntity.setPoiList(poiList);
        return searchResultEntity;
    }

    /**
     * 将批量沿途搜索结果 SearchLineDeepInfoResult 转换为 SearchResultEntity
     */
    public SearchResultEntity mapFromLineDeepInfoSearchResult(SearchRequestParameter requestParameterBuilder, SearchLineDeepInfoResult result) {
        SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, result.code, result.message);

        List<ServiceAreaInfo> serviceAreaInfoList = new ArrayList<>();
        List<ParkingInfo> parkingInfoList = new ArrayList<>();
        List<ChargeInfo> chargeInfoList = new ArrayList<>();
        List<GasStationInfo> stationList = new ArrayList<>();

        Optional.ofNullable(result.data).orElse(new ArrayList<>()).forEach(linePoiBase -> {
            switch (linePoiBase.queryType) {
                case LineDeepQueryType.eServiceArea:
                    serviceAreaInfoList.add(mapServiceAreaInfo((LinePoiServiceAreaInfo) linePoiBase));
                    break;
                case LineDeepQueryType.eCharing:
                    chargeInfoList.add(mapChargeInfo((LinePoiChargeInfo) linePoiBase));
                    break;
                case LineDeepQueryType.eGasStation:
                    stationList.add(mapGasStationInfo((LinePoiGasStationInfo) linePoiBase));
                    break;
                case LineDeepQueryType.eParkRecommend:
                    parkingInfoList.add(mapParkingInfo((LinePoiParkRecommendInfo) linePoiBase));
                    break;
                case LineDeepQueryType.AUTO_UNKNOWN_ERROR:
                    Logger.d(SEARCH_SERVICE_TAG, "未知错误类型");
                    break;
                default:
                    Logger.d(SEARCH_SERVICE_TAG, "未处理的类型: " + linePoiBase.queryType);
                    break;
            }
        });

        PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                .setServiceAreaInfoList(serviceAreaInfoList)
                .setChargeInfoList(chargeInfoList)
                .setStationList(stationList)
                .setParkingInfoList(parkingInfoList);

        searchResultEntity.setPoiList(List.of(poiInfoEntity));
        return searchResultEntity;
    }

    private ServiceAreaInfo mapServiceAreaInfo(LinePoiServiceAreaInfo linePoiServiceAreaInfo) {
        List<ServiceAreaInfo.ServiceAreaChild> serviceAreaChildList = linePoiServiceAreaInfo.children.stream()
                .map(this::mapServiceAreaChild)
                .collect(Collectors.toList());

        return new ServiceAreaInfo()
                .setName(linePoiServiceAreaInfo.name)
                .setPoiId(linePoiServiceAreaInfo.poiId)
                .setQueryType(linePoiServiceAreaInfo.queryType)
                .setAddress(linePoiServiceAreaInfo.address)
                .setTypeCode(linePoiServiceAreaInfo.typecode)
                .setBrand(linePoiServiceAreaInfo.brand)
                .setServiceStar(linePoiServiceAreaInfo.serviceStar)
                .setBuilding(linePoiServiceAreaInfo.building)
                .setServiceAreaChildList(serviceAreaChildList);
    }

    private ServiceAreaInfo.ServiceAreaChild mapServiceAreaChild(LinePoiServiceAreaChild linePoiServiceAreaChild) {
        return new ServiceAreaInfo.ServiceAreaChild()
                .setId(linePoiServiceAreaChild.childBase.id)
                .setName(linePoiServiceAreaChild.childBase.name)
                .setMinMame(linePoiServiceAreaChild.childBase.minMame)
                .setParentId(linePoiServiceAreaChild.childBase.parentId)
                .setTypeCode(linePoiServiceAreaChild.childBase.typecode)
                .setBusiness(linePoiServiceAreaChild.childBase.business)
                .setTag(linePoiServiceAreaChild.childBase.tag)
                .setDiscount(linePoiServiceAreaChild.discount)
                .setGasType(linePoiServiceAreaChild.gasType);
    }

    private ChargeInfo mapChargeInfo(LinePoiChargeInfo linePoiChargeInfo) {
        return new ChargeInfo()
                .setQueryType(linePoiChargeInfo.queryType)
                .setPoiId(linePoiChargeInfo.poiId)
                .setName(linePoiChargeInfo.name)
                .setTypeCode(linePoiChargeInfo.typecode)
                .setSlow_free(linePoiChargeInfo.slow_free)
                .setSlow_total(linePoiChargeInfo.slow_total)
                .setFast_free(linePoiChargeInfo.fast_free)
                .setFast_total(linePoiChargeInfo.fast_total)
                .setSuperFree(linePoiChargeInfo.superFree)
                .setSuperTotal(linePoiChargeInfo.superTotal);
    }

    private GasStationInfo mapGasStationInfo(LinePoiGasStationInfo linePoiGasStationInfo) {
        return new GasStationInfo()
                .setQueryType(linePoiGasStationInfo.queryType)
                .setPoiId(linePoiGasStationInfo.poiId)
                .setName(linePoiGasStationInfo.name)
                .setTypeCode(linePoiGasStationInfo.typecode)
                .setPrice("7.9")
                .setDiscount(linePoiGasStationInfo.discount);
    }

    private ParkingInfo mapParkingInfo(LinePoiParkRecommendInfo linePoiParkRecommendInfo) {
        return new ParkingInfo()
                .setQueryType(linePoiParkRecommendInfo.queryType)
                .setPoiId(linePoiParkRecommendInfo.poiId)
                .setName(linePoiParkRecommendInfo.name)
                .setTypeCode(linePoiParkRecommendInfo.typecode)
                .setAddress(linePoiParkRecommendInfo.address);
    }


    /**
     * 创建基础的 SearchResultEntity
     */
    private SearchResultEntity createBaseResultEntity(SearchRequestParameter requestParameterBuilder, int code, String message) {
        return new SearchResultEntity()
                .setCode(code)
                .setMessage(message)
                .setKeyword(requestParameterBuilder.getKeyword())
                .setPageNum(requestParameterBuilder.getPage())
                .setSearchType(requestParameterBuilder.getSearchType());
    }

    private String formatDistanceArrayInternal(int distance) {
        String[] distanceArray = ConvertUtils.formatDistanceArray(AppContext.mContext, distance);
        return distanceArray[0] + distanceArray[1];
    }

    private boolean isParking(String typeCode) {
        /* 停车场类型
         * 150903
         * 150904
         * 150905
         * 150906
         * 150907
         * 150908
         * 150909
         */
        String regex = "^150900|15090[3-9]$";
        return Pattern.matches(regex, typeCode);
    }
}