package com.sgm.navi.service.adapter.search.bls;


import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord2DDouble;
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
import com.autonavi.gbl.search.model.SearchBatchPoiDetailInfo;
import com.autonavi.gbl.search.model.SearchBatchPoiDetailResult;
import com.autonavi.gbl.search.model.SearchCategoryInfo;
import com.autonavi.gbl.search.model.SearchChildCategoryInfo;
import com.autonavi.gbl.search.model.SearchDeepInfoResult;
import com.autonavi.gbl.search.model.SearchDistrict;
import com.autonavi.gbl.search.model.SearchEnroutePoiInfo;
import com.autonavi.gbl.search.model.SearchEnrouteResult;
import com.autonavi.gbl.search.model.SearchLabelInfo;
import com.autonavi.gbl.search.model.SearchLineDeepInfoResult;
import com.autonavi.gbl.search.model.SearchNearestResult;
import com.autonavi.gbl.search.model.SearchPoiChildInfo;
import com.autonavi.gbl.search.model.SearchPoiInfo;
import com.autonavi.gbl.search.model.SearchPolygonBound;
import com.autonavi.gbl.search.model.SearchSuggestionPoiChildTip;
import com.autonavi.gbl.search.model.SearchSuggestionPoiTip;
import com.autonavi.gbl.search.model.SearchTipsCityInfo;
import com.autonavi.gbl.search.model.SuggestionSearchResult;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.BuildConfig;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.search.ChargeInfo;
import com.sgm.navi.service.define.search.ChildInfo;
import com.sgm.navi.service.define.search.CityInfo;
import com.sgm.navi.service.define.search.GasStationInfo;
import com.sgm.navi.service.define.search.LabelInfo;
import com.sgm.navi.service.define.search.ParkingInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchCategoryLocalInfo;
import com.sgm.navi.service.define.search.SearchChildCategoryLocalInfo;
import com.sgm.navi.service.define.search.SearchParkInOutInfo;
import com.sgm.navi.service.define.search.SearchRequestParameter;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.define.search.SearchRetainParamInfo;
import com.sgm.navi.service.define.search.SearchTipsCityLocalInfo;
import com.sgm.navi.service.define.search.SearchTipsLocalInfo;
import com.sgm.navi.service.define.search.ServiceAreaInfo;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * @author baipeg0904
 * @version \$Revision1.0\$
 * SearchResultMapper类负责将搜索结果映射到特定的数据结构。
 */
public final class SearchResultMapper {
    private static final String NUMBER_FORMAT_EXCEPTION = "NumberFormatException :";
    private static final String PARKINGERROR = "parkings is null";
    private final static class Holder {
        private static final SearchResultMapper INSTANCE = new SearchResultMapper();
    }

    private SearchResultMapper() {
    }

    public static SearchResultMapper getInstance() {
        return Holder.INSTANCE;
    }

    /**
     * 将预搜索结果 SuggestionSearchResult 转换为 SearchResultEntity
     *
     * @param requestParameterBuilder 搜索请求参数
     * @param result                  预搜索结果
     * @return SearchResultEntity
     */
    public SearchResultEntity mapFromSuggestionSearchResult(final SearchRequestParameter requestParameterBuilder,
                                                            final SuggestionSearchResult result) {
        final SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, result.code, result.message);
        final List<PoiInfoEntity> poiList = Optional.ofNullable(result.tipList)
                .orElse(new ArrayList<>())
                .stream()
                .map(this::mapSuggestionPoiTip)
                .collect(Collectors.toList());
        searchResultEntity.setPoiList(poiList);
        if (ConvertUtils.isEmpty(poiList) && !ConvertUtils.isEmpty(result.cityList)) {
            final List<PoiInfoEntity> cityList = Optional.ofNullable(result.cityList.get(0).tipList)
                    .orElse(new ArrayList<>())
                    .stream()
                    .map(this::mapSuggestionPoiTip)
                    .collect(Collectors.toList());
            searchResultEntity.setPoiList(cityList);
        }
        return searchResultEntity;
    }

    /**
     * 映射 SearchSuggestionPoiTip 到 PoiInfoEntity
     * @param suggestionPoiTip 搜索建议 POI 提示
     * @return PoiInfoEntity
     */
    private PoiInfoEntity mapSuggestionPoiTip(final SearchSuggestionPoiTip suggestionPoiTip) {
        final GeoPoint point = new GeoPoint();
        point.setLat(suggestionPoiTip.basicInfo.location.lat);
        point.setLon(suggestionPoiTip.basicInfo.location.lon);
        //子节点信息
        final List<ChildInfo> childInfoList = Optional.ofNullable(suggestionPoiTip.basicInfo.childInfoList)
                .orElse(new ArrayList<>())
                .stream()
                .map(this::mapSearchSuggestionPoiChildTip)
                .collect(Collectors.toList());

        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "mapSuggestionPoiTip childList: " + suggestionPoiTip.basicInfo.childInfoList);
        return new PoiInfoEntity()
                .setPid(suggestionPoiTip.basicInfo.poiId)
                .setName(suggestionPoiTip.basicInfo.name)
                .setAddress(suggestionPoiTip.basicInfo.address)
                .setDistance(formatDistanceArrayInternal(ConvertUtils.double2int(suggestionPoiTip.basicInfo.distance)))
                .setSort_distance(ConvertUtils.double2int(suggestionPoiTip.basicInfo.distance))
                .setChildInfoList(childInfoList)
                .setPoint(point);
    }

    /**
     * 映射 SearchPoiChildInfo 到 ChildInfo
     * @param searchPoiChildInfo SearchPoiChildInfo
     * @return ChildInfo
     */
    private ChildInfo mapSearchSuggestionPoiChildTip(final SearchSuggestionPoiChildTip searchPoiChildInfo) {
        return new ChildInfo()
                .setName(searchPoiChildInfo.name)
                .setShortName(searchPoiChildInfo.shortName)
                .setRatio(searchPoiChildInfo.ratio)
                .setPoiId(searchPoiChildInfo.poiId)
                .setLocation(new GeoPoint(searchPoiChildInfo.location.lon, searchPoiChildInfo.location.lat))
                .setAddress(searchPoiChildInfo.address);
    }

    /**
     * 限制列表的最大size
     * @param list 源数据
     * @param maxSize 支持的最大size
     * @param <T> list变量
     */
    public <T> void limitListToSize(final List<T> list, final int maxSize) {
        if (list.size() > maxSize) {
            final List<T> subList = new ArrayList<>(list.subList(0, maxSize));
            list.clear();
            list.addAll(subList);
        }
    }

    /**
     * 将关键字搜索结果 KeywordSearchResultV2 转换为 SearchResultEntity
     *
     * @param requestParameterBuilder 请求参数构建器
     * @param result                  搜索结果
     * @return SearchResultEntity
     */
    public SearchResultEntity mapFromKeywordSearchResultV2(final SearchRequestParameter requestParameterBuilder, final KeywordSearchResultV2 result) {
        final SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, result.code, result.message);
        final List<PoiInfoEntity> poiList = Optional.ofNullable(result.poiList)
                .orElse(new ArrayList<>())
                .stream()
                .map(this::mapSearchPoiInfo)
                .collect(Collectors.toList());
        if (requestParameterBuilder.getSearchType() == AutoMapConstant.SearchType.TERMINAL_PARK_AROUND_SEARCH) {
            //终点停车场列表最多提供8个
            limitListToSize(poiList, 8);
        }
        searchResultEntity.setMaxPageNum(result.total / 10 + 1);
        searchResultEntity.setTotal(result.total);
        searchResultEntity.setPoiList(poiList);
        searchResultEntity.setPoiType(result.poiType);//0=离线数据，1=在线数据
        if(!ConvertUtils.isNull(result.lqii)){
            searchResultEntity.setQueryTypeList(result.lqii.queryTypeList);
        }
        if (ConvertUtils.isEmpty(poiList) && !ConvertUtils.isEmpty(result.poiLocres)) {
            final List<PoiInfoEntity> cityList = Optional.ofNullable(result.poiLocres.citylist)
                    .orElse(new ArrayList<>())
                    .stream()
                    .map(this::mapSearchDistrictPoiInfo)
                    .collect(Collectors.toList());
            searchResultEntity.setPoiList(cityList);
        }

        //获取筛选分类数据
        if (result.classify != null) {
            searchResultEntity.setRetain(result.classify.retainState);//筛选搜索需要的信息
            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "mapFromKeywordSearchResultV2 retainState: " + result.classify.retainState);
            if (result.classify.classifyItemInfo != null) {
                //二筛参数列表
                final List<SearchCategoryLocalInfo> categoryLocalInfoList2 = Optional.ofNullable(result.classify.classifyItemInfo.level2CategoryInfoList)
                        .orElse(new ArrayList<>())
                        .stream()
                        .map(this::mapSearchCategoryInfo)
                        .collect(Collectors.toList());
                if (!categoryLocalInfoList2.isEmpty()) {
                    for (int i = 0; i < categoryLocalInfoList2.size(); i++) {
                        final List<SearchChildCategoryLocalInfo> level1Infos = Optional.ofNullable(result.classify.classifyItemInfo.level2CategoryInfoList.
                                        get(i).childCategoryInfo)
                                .orElse(new ArrayList<>())
                                .stream()
                                .map(this::mapSearchChildCategoryInfo)
//                                .filter(this::shouldIncludeInResult)//过滤value或者name值不存在的item
                                .collect(Collectors.toList());
                        categoryLocalInfoList2.get(i).setCategoryLocalInfos(level1Infos);
                        if (!level1Infos.isEmpty()) {
                            for (int j = 0; j < level1Infos.size(); j++) {
                                final List<SearchChildCategoryLocalInfo> level1ChildInfos = Optional.ofNullable(
                                                result.classify.classifyItemInfo.level2CategoryInfoList.
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

                //一筛参数列表
                final List<SearchCategoryLocalInfo> categoryLocalInfoList = Optional.ofNullable(result.classify.classifyItemInfo.categoryInfoList)
                        .orElse(new ArrayList<>())
                        .stream()
                        .map(this::mapSearchCategoryInfo)
                        .collect(Collectors.toList());
                if (!categoryLocalInfoList.isEmpty()) {
                    for (int i = 0; i < categoryLocalInfoList.size(); i++) {
                        final List<SearchChildCategoryLocalInfo> level1Infos = Optional.ofNullable(result.classify.classifyItemInfo.categoryInfoList.
                                        get(i).childCategoryInfo)
                                .orElse(new ArrayList<>())
                                .stream()
                                .map(this::mapSearchChildCategoryInfo)
//                                .filter(this::shouldIncludeInResult)//过滤value或者name值不存在的item
                                .collect(Collectors.toList());
                        if (!level1Infos.isEmpty()) {
                            for (int j = 0; j < level1Infos.size(); j++) {
                                final List<SearchChildCategoryLocalInfo> level1ChildInfos = Optional.ofNullable(
                                        result.classify.classifyItemInfo.categoryInfoList.
                                                get(i).childCategoryInfo.
                                                get(j).childCategoryInfoList)
                                        .orElse(new ArrayList<>())
                                        .stream()
                                        .map(this::mapSearchChildCategoryInfo)
//                                        .filter(this::shouldIncludeInResult)//过滤value或者name值不存在的item
                                        .collect(Collectors.toList());
                                level1Infos.get(j).setCategoryLocalInfos(level1ChildInfos);
                            }
                            // 过滤掉没有子节点且自身没有value的数据
                            level1Infos.removeIf(info -> ConvertUtils.isEmpty(info.getCategoryLocalInfos()) && ConvertUtils.isEmpty(info.getValue()));
                        }
                        categoryLocalInfoList.get(i).setCategoryLocalInfos(level1Infos);
                    }
                }
                //测试log，测试时放开
//                for (SearchCategoryLocalInfo searchCategoryLocalInfo : categoryLocalInfoList2) {
//                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "mapFromKeywordSearchResultV2 name: " + searchCategoryLocalInfo.getName()
//                            + " checked: " + searchCategoryLocalInfo.getChecked());
//                    for (SearchChildCategoryLocalInfo searchChildCategoryLocalInfo : searchCategoryLocalInfo.getCategoryLocalInfos()) {
//                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "mapFromKeywordSearchResultV2 level name: "
//                        + searchChildCategoryLocalInfo.getName()
//                                + " checked: " + searchChildCategoryLocalInfo.getChecked()
//                                + " value: " + searchChildCategoryLocalInfo.getValue());
//                        for (SearchChildCategoryLocalInfo searchChildCategoryLocalInfo1 : searchChildCategoryLocalInfo.getCategoryLocalInfos()) {
//                            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "mapFromKeywordSearchResultV2 level child name: "
//                            + searchChildCategoryLocalInfo1.getName()
//                                    + " checked: " + searchChildCategoryLocalInfo1.getChecked()
//                                    + " value: " + searchChildCategoryLocalInfo1.getValue());
//                        }
//                    }
//                }
//                for (SearchCategoryLocalInfo searchCategoryLocalInfo : categoryLocalInfoList) {
//                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "mapFromKeywordSearchResultV2 name: " + searchCategoryLocalInfo.getName()
//                            + " checked: " + searchCategoryLocalInfo.getChecked());
//                    for (SearchChildCategoryLocalInfo searchChildCategoryLocalInfo : searchCategoryLocalInfo.getCategoryLocalInfos()) {
//                        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "mapFromKeywordSearchResultV2 level name: "
//                                + searchChildCategoryLocalInfo.getName()
//                                + " checked: " + searchChildCategoryLocalInfo.getChecked()
//                                + " value: " + searchChildCategoryLocalInfo.getValue());
//                        for (SearchChildCategoryLocalInfo searchChildCategoryLocalInfo1 : searchChildCategoryLocalInfo.getCategoryLocalInfos()) {
//                            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "mapFromKeywordSearchResultV2 level child name: "
//                                    + searchChildCategoryLocalInfo1.getName()
//                                    + " checked: " + searchChildCategoryLocalInfo1.getChecked()
//                                    + " value: " + searchChildCategoryLocalInfo1.getValue());
//                        }
//                    }
//                }
                searchResultEntity.setLocalInfoList(categoryLocalInfoList);
                searchResultEntity.setLevel2LocalInfoList(categoryLocalInfoList2);
            }
        }

        //回传参数，POI详情搜索必传，来自于关键字搜索结果,存储后用于之后POI详情搜索
        if (result.lqii != null && result.lqii.tipsInfo != null) {
            final List<SearchTipsCityLocalInfo> cityLocalInfoList = Optional.ofNullable(result.lqii.tipsInfo.optionalCityList)
                    .orElse(new ArrayList<>())
                    .stream()
                    .map(this::mapSearchTipsCityInfo)
                    .collect(Collectors.toList());
            final SearchTipsLocalInfo searchTipsLocalInfo = new SearchTipsLocalInfo()
                    .setSceneType(result.lqii.tipsInfo.sceneType)
                    .setCityName(result.lqii.tipsInfo.cityName)
                    .setKeyword(result.lqii.tipsInfo.keyword)
                    .setOptionalKeywordList(result.lqii.tipsInfo.optionalKeywordList)
                    .setRetainState(result.lqii.tipsInfo.retainState)
                    .setOptionalCityList(cityLocalInfoList);

            final SearchRetainParamInfo searchRetainParamInfo = new SearchRetainParamInfo()
                    .setKeywordBizType(result.retainParam.keywordBizType)
                    .setSearchTipsLocalInfo(searchTipsLocalInfo);
            for (PoiInfoEntity infoEntity : poiList) {
                infoEntity.setRetainParam(searchRetainParamInfo);
            }
        }
        return searchResultEntity;
    }

    /**
     * 是否需要被过滤
     * @param localInfo SearchChildCategoryLocalInfo
     * @return ture or false
     */
    private boolean shouldIncludeInResult(final SearchChildCategoryLocalInfo localInfo) {
        return !ConvertUtils.isEmpty(localInfo.getName()) && !ConvertUtils.isEmpty(localInfo.getValue());
    }

    /**
     * 映射 SearchCategoryInfo 到 SearchCategoryLocalInfo
     * @param searchCategoryInfo SearchCategoryInfo
     * @return SearchCategoryLocalInfo
     */
    private SearchCategoryLocalInfo mapSearchCategoryInfo(final SearchCategoryInfo searchCategoryInfo) {
        return new SearchCategoryLocalInfo()
                .setName(searchCategoryInfo.baseInfo.name)
                .setChecked(searchCategoryInfo.baseInfo.checked);
    }

    /**
     * 映射 SearchChildCategoryInfo 到 SearchChildCategoryLocalInfo
     * @param searchChildCategoryInfo SearchChildCategoryInfo
     * @return SearchChildCategoryLocalInfo
     */
    private SearchChildCategoryLocalInfo mapSearchChildCategoryInfo(final SearchChildCategoryInfo searchChildCategoryInfo) {
        return new SearchChildCategoryLocalInfo()
                .setName(searchChildCategoryInfo.baseInfo.name)
                .setValue(searchChildCategoryInfo.baseInfo.value)
                .setChecked(searchChildCategoryInfo.baseInfo.checked);
    }

    /**
     * 映射 SearchTipsCityInfo 到 SearchTipsCityLocalInfo
     * @param searchTipsCityInfo SearchTipsCityInfo
     * @return SearchTipsCityLocalInfo
     */
    private SearchTipsCityLocalInfo mapSearchTipsCityInfo(final SearchTipsCityInfo searchTipsCityInfo) {
        return new SearchTipsCityLocalInfo()
                .setAdcode(searchTipsCityInfo.adcode)
                .setIndex(searchTipsCityInfo.index)
                .setName(searchTipsCityInfo.name);
    }

    /**
     * 映射 SearchPoiChildInfo 到 ChildInfo
     * @param searchPoiChildInfo SearchPoiChildInfo
     * @return ChildInfo
     */
    private ChildInfo mapSearchPoiChildInfo(final SearchPoiChildInfo searchPoiChildInfo) {
        return new ChildInfo()
                .setName(searchPoiChildInfo.name)
                .setShortName(searchPoiChildInfo.shortName)
                .setRatio(searchPoiChildInfo.ratio)
                .setLabel(searchPoiChildInfo.label)
                .setPoiId(searchPoiChildInfo.poiId)
                .setMChildType(searchPoiChildInfo.childType)
                .setLocation(new GeoPoint(searchPoiChildInfo.location.lon, searchPoiChildInfo.location.lat))
                .setAddress(searchPoiChildInfo.address);
    }

    /**
     * 获取停车场列表
     * @param searchPoiInfo SearchPoiInfo
     * @return 停车场列表
     */
    private List<ParkingInfo> getParkingList(final SearchPoiInfo searchPoiInfo) {
        //停车场信息
        final List<ParkingInfo> parkingInfoList = new ArrayList<>();
        // 停车场出入口信息
        final List<SearchParkInOutInfo> searchParkInOutInfos = searchPoiInfo.parkingInfo.inoutInfoList
                .stream()
                .map(inoutInfo -> new SearchParkInOutInfo()
                        .setEntExitId(inoutInfo.entExitId)
                        .setKeytype(inoutInfo.keytype)
                        .setX(inoutInfo.x)
                        .setY(inoutInfo.y))
                .collect(Collectors.toList());
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "free is: " + searchPoiInfo.parkingInfo.dynamicParking.spaceFree
                    + " ,total: " + searchPoiInfo.parkingInfo.dynamicParking.spaceTotal);
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "free1 is: " + searchPoiInfo.parkingInfo.space
                + " ,total1: " + searchPoiInfo.parkingInfo.spaceFree);
        final ParkingInfo parkingInfo = new ParkingInfo()
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
        return parkingInfoList;
    }

    /**
     * 映射 SearchDistrict 到 PoiInfoEntity
     * @param searchDistrict SearchDistrict
     * @return PoiInfoEntity
     */
    private PoiInfoEntity mapSearchDistrictPoiInfo(final SearchDistrict searchDistrict) {
        final GeoPoint point = new GeoPoint();
        point.setLat(searchDistrict.poi_loc.lat);
        point.setLon(searchDistrict.poi_loc.lon);
        //区域边界点信息
        final ArrayList<ArrayList<GeoPoint>> poiAoiBounds = new ArrayList<>();
        for (SearchPolygonBound coord2DDouble : searchDistrict.polygonBounds) {
            final ArrayList<GeoPoint> points = new ArrayList<>();
            for (Coord2DDouble coord2D : coord2DDouble.points) {
                final GeoPoint geoPoint = new GeoPoint(coord2D.lon, coord2D.lat);
                points.add(geoPoint);
            }
            poiAoiBounds.add(points);
        }
        return new PoiInfoEntity()
                .setIsLocres(true)
                .setPid("C" + point.getLon() + "_" + point.getLat())
                .setName(searchDistrict.name)
                .setAddress(searchDistrict.address)
                .setPoint(point)
                .setAdCode(searchDistrict.adcode)
                .setMPoiAoiBounds(poiAoiBounds);
    }

    /**
     * 映射 SearchPoiInfo 到 PoiInfoEntity
     * @param searchPoiInfo SearchPoiInfo
     * @return PoiInfoEntity
     */
    private PoiInfoEntity mapSearchPoiInfo(final SearchPoiInfo searchPoiInfo) {
        final GeoPoint point = new GeoPoint();
        point.setLat(searchPoiInfo.basicInfo.location.lat);
        point.setLon(searchPoiInfo.basicInfo.location.lon);
        //加油站信息
        final List<GasStationInfo> gasStationInfos = new ArrayList<>();
        for (int i = 0; i < searchPoiInfo.gasInfo.typeList.size(); i++) {
            gasStationInfos.add(new GasStationInfo()
                    .setType(searchPoiInfo.gasInfo.typeList.get(i))
                    .setPrice(searchPoiInfo.gasInfo.priceList.get(i)));
            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "gas type is: " + searchPoiInfo.gasInfo.typeList.get(i)
                    + "gas price is: " + searchPoiInfo.gasInfo.priceList.get(i));
        }
        //充电站信息
        final List<ChargeInfo> chargeInfos = new ArrayList<>();
        final int slowFree = TextUtils.isEmpty(searchPoiInfo.chargingStationInfo.slow_free) ?
                0 : Integer.parseInt(searchPoiInfo.chargingStationInfo.slow_free);
        final int slowTotal = TextUtils.isEmpty(searchPoiInfo.chargingStationInfo.slow_total) ?
                0 : Integer.parseInt(searchPoiInfo.chargingStationInfo.slow_total);
        final int fastFree = TextUtils.isEmpty(searchPoiInfo.chargingStationInfo.fast_free) ?
                0 : Integer.parseInt(searchPoiInfo.chargingStationInfo.fast_free);
        final int fastTotal = TextUtils.isEmpty(searchPoiInfo.chargingStationInfo.fast_total) ?
                0 : Integer.parseInt(searchPoiInfo.chargingStationInfo.fast_total);
        final ChargeInfo chargeInfo = new ChargeInfo()
                .setSlow_free(slowFree)
                .setSlow_total(slowTotal)
                .setFast_free(fastFree)
                .setFast_total(fastTotal)
                .setCurrentElePrice(searchPoiInfo.chargingStationInfo.current_ele_price)
                .setCurrentServicePrice(searchPoiInfo.chargingStationInfo.parkPrice)
                .setMBrand(searchPoiInfo.chargingStationInfo.brand_desc)
                .setMLatestChargeTimestamp(searchPoiInfo.chargingStationInfo.latestChargeTimestamp)
                .setMSearchTimestamp(searchPoiInfo.chargingStationInfo.searchTimestamp);
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "brand is: " + searchPoiInfo.chargingStationInfo.brand_desc
                    + " ,park_category: " + searchPoiInfo.chargingStationInfo.park_category+" ,current_ele_price: " + searchPoiInfo.chargingStationInfo.current_ele_price);
        for (ChargingPlugInfo chargingPlugInfo : searchPoiInfo.chargingStationInfo.plugsInfo) {
            if (chargingPlugInfo.plugType == AutoMapConstant.PLUG_TYPE_SLOW) {
                chargeInfo.setSlowVolt(chargingPlugInfo.slowVoltage)
                        .setSlowPower(chargingPlugInfo.slowPower);
            }
            if (chargingPlugInfo.plugType == AutoMapConstant.PLUG_TYPE_FAST) {
                chargeInfo.setFastVolt(chargingPlugInfo.fastVoltage)
                        .setFastPower(chargingPlugInfo.fastPower);
            }
        }
        chargeInfos.add(chargeInfo);
        //子节点信息
        final List<ChildInfo> childInfoList = Optional.ofNullable(searchPoiInfo.childInfoList)
                .orElse(new ArrayList<>())
                .stream()
                .map(this::mapSearchPoiChildInfo)
                .collect(Collectors.toList());
        int childType = AutoMapConstant.ChildType.DEFAULT;
        if (!ConvertUtils.isEmpty(childInfoList)) {
            childType = AutoMapConstant.ChildType.HAS_CHILD_NO_GRAND;
        }
        //区域边界点信息
        final ArrayList<ArrayList<GeoPoint>> poiAoiBounds = new ArrayList<>();
        for (ArrayList<Coord2DDouble> coord2DDouble : searchPoiInfo.basicInfo.poiAoiBounds) {
            final ArrayList<GeoPoint> points = new ArrayList<>();
            for (Coord2DDouble coord2D : coord2DDouble) {
                final GeoPoint geoPoint = new GeoPoint(coord2D.lon, coord2D.lat);
                points.add(geoPoint);
            }
            poiAoiBounds.add(points);
        }
        //道路边界点信息
        final ArrayList<ArrayList<GeoPoint>> roadBounds = new ArrayList<>();
        for (ArrayList<Coord2DDouble> coord2DDouble : searchPoiInfo.basicInfo.roadPolygonBounds) {
            final ArrayList<GeoPoint> points = new ArrayList<>();
            for (Coord2DDouble coord2D : coord2DDouble) {
                final GeoPoint geoPoint = new GeoPoint(coord2D.lon, coord2D.lat);
                points.add(geoPoint);
            }
            roadBounds.add(points);
        }
        final List<LabelInfo> labelInfos = new ArrayList<>();
        for (SearchLabelInfo searchLabelInfo : searchPoiInfo.labelInfos) {
            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, " label info type: " + searchLabelInfo.type + " ,content: " + searchLabelInfo.content);
            final LabelInfo labelInfo = new LabelInfo()
                    .setMContent(searchLabelInfo.content)
                    .setMType(searchLabelInfo.type);
            labelInfos.add(labelInfo);
        }
        Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "typeCode is: " + searchPoiInfo.basicInfo.typeCode
                + " ;name is: " + searchPoiInfo.basicInfo.name + " ;searchPoiInfo.basicInfo.pid:" + searchPoiInfo.basicInfo.poiId
                + " :isFastest: " + searchPoiInfo.basicInfo.isFastest + " ;isClosest: " + searchPoiInfo.basicInfo.isClosest);
        return new PoiInfoEntity()
                .setPointTypeCode(searchPoiInfo.basicInfo.typeCode)
                .setPid(searchPoiInfo.basicInfo.poiId)
                .setName(searchPoiInfo.basicInfo.name)
                .setAddress(searchPoiInfo.basicInfo.address)
                .setDistance(formatDistanceArrayInternal(ConvertUtils.double2int(ConvertUtils.str2Double(searchPoiInfo.basicInfo.distance))))

                .setPoint(point)
                .setAdCode(searchPoiInfo.basicInfo.adcode)
                .setMCityCode(searchPoiInfo.basicInfo.cityCode)
                .setIndustry(searchPoiInfo.basicInfo.industry)
                .setPhone(searchPoiInfo.basicInfo.tel)
                .setImageUrl(searchPoiInfo.basicInfo.imageUrl)
                .setBusinessTime(searchPoiInfo.basicInfo.openTime)
                .setRating(searchPoiInfo.basicInfo.rating)
                .setAverageCost(searchPoiInfo.basicInfo.averageCost)
                .setPoiTag(isParking(searchPoiInfo.basicInfo.typeCode) ? "停车场" : searchPoiInfo.basicInfo.tag)
                .setParkingInfoList(getParkingList(searchPoiInfo))
                .setChildInfoList(childInfoList)
                .setMChildType(childType)
                .setStationList(gasStationInfos)
                .setSort_distance(ConvertUtils.str2Int(searchPoiInfo.basicInfo.distance))
                .setSort_rate(ConvertUtils.str2Int(searchPoiInfo.rankInfo.rankNo))
                .setSort_price(ConvertUtils.str2Int(searchPoiInfo.hotelInfo.priceLowest))
                .setMPoiAoiBounds(poiAoiBounds)
                .setMRoadPolygonBounds(roadBounds)
                .setChargeInfoList(chargeInfos)
                .setIsFastest(searchPoiInfo.basicInfo.isFastest)
                .setIsClosest(searchPoiInfo.basicInfo.isClosest);
    }

    /**
     * 将Poi id搜索结果 KeywordSearchResultV2 转换为 SearchResultEntity
     *
     * @param requestParameterBuilder SearchRequestParameter
     * @param result                   搜索结果
     * @return SearchResultEntity
     */
    public SearchResultEntity mapFromPoiDetailsSearchResult(final SearchRequestParameter requestParameterBuilder,
                                                            final KeywordSearchResultV2 result) {
        final SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, result.code, result.message);
        final List<PoiInfoEntity> poiList = Optional.ofNullable(result.poiList)
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
     *
     * @param requestParameterBuilder SearchRequestParameter
     * @param result                   搜索结果
     * @return SearchResultEntity
     */
    public SearchResultEntity mapFromGeoSearchResult(final SearchRequestParameter requestParameterBuilder, final SearchNearestResult result) {
        final SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, result.code, result.message);
        final GeoPoint poiPoint = new GeoPoint(requestParameterBuilder.getPoiLoc().getLon(), requestParameterBuilder.getPoiLoc().getLat());
        searchResultEntity.setPoiType(result.iPoiType);
        if (result.poi_list == null || result.poi_list.isEmpty()) {
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "GeoSearchResult poi_list is empty.");
            searchResultEntity.setPoiList(Collections.emptyList());
            searchResultEntity.setSearchType(AutoMapConstant.SearchType.GEO_SEARCH);
            PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
            poiInfoEntity.setPoint(poiPoint)
                    .setName(result.desc)
                    .setAddress(result.pos);
            if (ConvertUtils.equals("基础功能包", result.pos) || ConvertUtils.isEmpty(result.pos)) {
                poiInfoEntity.setAddress(result.city);
            }
            Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "GeoSearchResult desc: " + result.desc
                    + " ,pos: " + result.pos
                    + " ,city: " + result.city);
            searchResultEntity.setPoiList(Collections.singletonList(poiInfoEntity));
            return searchResultEntity;
        }

        // 获取第一个 POI 信息
        final NearestPoi poiItem = result.poi_list.get(0);

        // 构建 CityInfo
        final CityInfo cityInfo = buildCityInfo(result, poiPoint);

        // 构建 PoiInfoEntity
        final PoiInfoEntity poiInfoEntity = buildPoiInfoEntity(poiItem, poiPoint, cityInfo,result);

        // 设置 POI 列表并返回结果
        searchResultEntity.setPoiList(Collections.singletonList(poiInfoEntity));
        return searchResultEntity;
    }

    /**
     * 构建 CityInfo
     * @param result SearchNearestResult
     * @param poiPoint GeoPoint
     * @return CityInfo
     */
    private CityInfo buildCityInfo(final SearchNearestResult result, final GeoPoint poiPoint) {
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

    /**
     * 构建 PoiInfoEntity
     * @param poiItem NearestPoi
     * @param poiPoint GeoPoint
     * @param cityInfo  CityInfo
     * @param result SearchNearestResult
     * @return PoiInfoEntity
     */
    private PoiInfoEntity buildPoiInfoEntity(final NearestPoi poiItem, final GeoPoint poiPoint,
                                             final CityInfo cityInfo, final SearchNearestResult result) {
        final PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
        poiInfoEntity.setPointTypeCode(poiItem.typecode)
                .setDistance(formatDistanceArrayInternal(poiItem.distance))
                .setSort_price(poiItem.distance)
                .setPoint(poiPoint)
                .setAdCode(poiItem.nAdCode)
                .setPhone(poiItem.tel)
                .setBusinessTime("")
                .setCityInfo(cityInfo);
        if (!ConvertUtils.isEmpty(result.desc)) {
            poiInfoEntity.setName(result.desc);
        } else {
            poiInfoEntity.setName(poiItem.name);
        }
        if (!ConvertUtils.isEmpty(result.pos)) {
            poiInfoEntity.setAddress(result.pos);
        } else {
            poiInfoEntity.setAddress(poiItem.address);
        }
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "current " + poiPoint.getLon() + " " + poiPoint.getLat()
                + " result " + poiItem.point.lon + " " + poiItem.point.lat + " id: " + poiItem.poiid
                + " name: " + poiItem.name);
        if (poiPoint.getLon() == poiItem.point.lon && poiPoint.getLat() == poiItem.point.lat) {
            //如果搜索出的第一个poi点经纬度和逆地理搜索请求点的经纬度相同，则认为是同一个点，赋值pid
            poiInfoEntity.setPid(poiItem.poiid);
            poiInfoEntity.setName(poiItem.name);
            poiInfoEntity.setAddress(poiItem.address);
        }
        return poiInfoEntity;
    }


    /**
     * 将沿途搜索结果 SearchAlongWayResult 转换为 SearchResultEntity
     *
     * @param requestParameterBuilder SearchRequestParameter
     * @param result                   搜索结果
     * @return SearchResultEntity
     */
    public SearchResultEntity mapFromAlongWayResult(final SearchRequestParameter requestParameterBuilder, final SearchAlongWayResult result) {
        final SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, result.code, result.message);
        final List<PoiInfoEntity> poiList = Optional.ofNullable(result.pois)
                .orElse(new ArrayList<>())
                .stream()
                .map(this::mapSearchPoiInfo)
                .collect(Collectors.toList());
        searchResultEntity.setPoiList(poiList);
        return searchResultEntity;
    }


    /**
     * 将沿途搜索结果 NearestPoi 转换为 PoiInfoEntity
     * @param nearestPoi 搜索结果
     * @return PoiInfoEntity
     */
    private PoiInfoEntity mapSearchPoiInfo(final AlongWayPoi nearestPoi) {
        final GeoPoint point = new GeoPoint();
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
     * 将聚合搜索结果 AggregateDiningResult 转换为 PoiInfoEntity
     * @param result 搜索结果
     * @param poiList poi列表
     */
    private void mapFromAggregateDiningResult(final AggregateSearchResult result, final List<PoiInfoEntity> poiList) {
        if (result.dinings != null) {
            result.dinings.stream()
                    .filter(aggregateDiningResult -> aggregateDiningResult.poiInfo != null)
                    .forEach(aggregateDiningResult -> {
                        try {
                            final int distance = ConvertUtils.str2Int(aggregateDiningResult.poiInfo.distance);
                            final PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                                    .setPid(aggregateDiningResult.poiInfo.poiId)
                                    .setName(aggregateDiningResult.poiInfo.name)
                                    .setAddress(aggregateDiningResult.poiInfo.address)
                                    .setDistance(formatDistanceArrayInternal(distance))
                                    .setSort_distance(distance)
                                    .setPoint(new GeoPoint(aggregateDiningResult.poiInfo.location.lon, aggregateDiningResult.poiInfo.location.lat))
                                    .setAdCode(aggregateDiningResult.poiInfo.adcode);
                            poiList.add(poiInfoEntity);
                        } catch (NumberFormatException e) {
                            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, NUMBER_FORMAT_EXCEPTION + e.getMessage());
                        }
                    });
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, PARKINGERROR);
        }
    }

    /**
     * 将聚合搜索结果 AggregateScenicResult 转换为 PoiInfoEntity
     * @param result 搜索结果
     * @param poiList poi列表
     */
    private void mapFromAggregateScenicResult(final AggregateSearchResult result, final List<PoiInfoEntity> poiList) {
        if (result.scenics != null) {
            result.scenics.stream()
                    .filter(aggregateDiningResult -> aggregateDiningResult.poiInfo != null)
                    .forEach(aggregateDiningResult -> {
                        try {
                            final int distance = ConvertUtils.str2Int(aggregateDiningResult.poiInfo.distance);
                            final PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                                    .setPid(aggregateDiningResult.poiInfo.poiId)
                                    .setName(aggregateDiningResult.poiInfo.name)
                                    .setAddress(aggregateDiningResult.poiInfo.address)
                                    .setDistance(formatDistanceArrayInternal(distance))
                                    .setSort_distance(distance)
                                    .setPoint(new GeoPoint(aggregateDiningResult.poiInfo.location.lon,
                                            aggregateDiningResult.poiInfo.location.lat))
                                    .setAdCode(aggregateDiningResult.poiInfo.adcode);
                            poiList.add(poiInfoEntity);
                        } catch (NumberFormatException e) {
                            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, NUMBER_FORMAT_EXCEPTION + e.getMessage());
                        }
                    });
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, PARKINGERROR);
        }
    }

    /**
     * 将聚合搜索结果 AggregateMallResult 转换为 PoiInfoEntity
     * @param result 搜索结果
     * @param poiList poi列表
     */
    private void mapFromAggregateMallResult(final AggregateSearchResult result, final List<PoiInfoEntity> poiList) {
        if (result.malls != null) {
            result.malls.stream()
                    .filter(aggregateDiningResult -> aggregateDiningResult.poiInfo != null)
                    .forEach(aggregateDiningResult -> {
                        try {
                            final int distance = ConvertUtils.str2Int(aggregateDiningResult.poiInfo.distance);
                            final PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                                    .setPid(aggregateDiningResult.poiInfo.poiId)
                                    .setName(aggregateDiningResult.poiInfo.name)
                                    .setAddress(aggregateDiningResult.poiInfo.address)
                                    .setDistance(formatDistanceArrayInternal(distance))
                                    .setSort_distance(distance)
                                    .setPoint(new GeoPoint(aggregateDiningResult.poiInfo.location.lon,
                                            aggregateDiningResult.poiInfo.location.lat))
                                    .setAdCode(aggregateDiningResult.poiInfo.adcode);
                            poiList.add(poiInfoEntity);
                        } catch (NumberFormatException e) {
                            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, NUMBER_FORMAT_EXCEPTION + e.getMessage());
                        }
                    });
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, PARKINGERROR);
        }
    }

    /**
     * 将聚合搜索 AggregateSearchResult 转换为 SearchResultEntity
     * @param requestParameterBuilder SearchRequestParameter
     * @param result                   搜索结果
     * @return SearchResultEntity
     */
    public SearchResultEntity mapFromAggregateSearchResult(final SearchRequestParameter requestParameterBuilder, final AggregateSearchResult result) {
        final SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, result.code, result.message);
        final List<PoiInfoEntity> poiList = new ArrayList<>();
        switch (requestParameterBuilder.getType()) {
            case AutoMapConstant.AggregateKeywordType.DINING:
                mapFromAggregateDiningResult(result, poiList);
                break;
            case AutoMapConstant.AggregateKeywordType.SCENIC:
                mapFromAggregateScenicResult(result, poiList);
                break;
            case AutoMapConstant.AggregateKeywordType.MALL:
                mapFromAggregateMallResult(result, poiList);
                break;
            case AutoMapConstant.AggregateKeywordType.CHARGING:
                if (result.chargings != null) {
                    result.chargings.stream()
                            .filter(aggregateDiningResult -> aggregateDiningResult.poiInfo != null)
                            .forEach(aggregateDiningResult -> {
                                try {
                                    final int distance = ConvertUtils.str2Int(aggregateDiningResult.poiInfo.distance);
                                    final PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                                            .setPid(aggregateDiningResult.poiInfo.poiId)
                                            .setName(aggregateDiningResult.poiInfo.name)
                                            .setAddress(aggregateDiningResult.poiInfo.address)
                                            .setDistance(formatDistanceArrayInternal(distance))
                                            .setSort_distance(distance)
                                            .setPoint(new GeoPoint(aggregateDiningResult.poiInfo.location.lon,
                                                    aggregateDiningResult.poiInfo.location.lat))
                                            .setAdCode(aggregateDiningResult.poiInfo.adcode);
                                    poiList.add(poiInfoEntity);
                                } catch (NumberFormatException e) {
                                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, NUMBER_FORMAT_EXCEPTION + e.getMessage());
                                }
                            });
                } else {
                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, PARKINGERROR);
                }
                break;
            case AutoMapConstant.AggregateKeywordType.PARKING:
                if (result.parkings != null) {
                    result.parkings.stream()
                            .filter(aggregateDiningResult -> aggregateDiningResult.poiInfo != null)
                            .forEach(aggregateDiningResult -> {
                                try {
                                    final int distance = ConvertUtils.str2Int(aggregateDiningResult.poiInfo.distance);
                                    final PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                                            .setPid(aggregateDiningResult.poiInfo.poiId)
                                            .setName(aggregateDiningResult.poiInfo.name)
                                            .setAddress(aggregateDiningResult.poiInfo.address)
                                            .setDistance(formatDistanceArrayInternal(distance))
                                            .setSort_distance(distance)
                                            .setPoint(new GeoPoint(aggregateDiningResult.poiInfo.location.lon,
                                                    aggregateDiningResult.poiInfo.location.lat))
                                            .setAdCode(aggregateDiningResult.poiInfo.adcode);
                                    poiList.add(poiInfoEntity);
                                } catch (NumberFormatException e) {
                                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, NUMBER_FORMAT_EXCEPTION + e.getMessage());
                                }
                            });
                } else {
                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, PARKINGERROR);
                }
                break;
            case AutoMapConstant.AggregateKeywordType.BATHROOM:
                if (result.others != null) {
                    result.others.stream()
                            .filter(aggregateDiningResult -> aggregateDiningResult.bathroom != null)
                            .forEach(aggregateDiningResult -> {
                                try {
                                    final int distance = ConvertUtils.str2Int(aggregateDiningResult.bathroom.distance);
                                    final PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                                            .setPid(aggregateDiningResult.bathroom.poiId)
                                            .setName(aggregateDiningResult.bathroom.name)
                                            .setAddress(aggregateDiningResult.bathroom.address)
                                            .setDistance(formatDistanceArrayInternal(distance))
                                            .setSort_distance(distance)
                                            .setPoint(new GeoPoint(aggregateDiningResult.bathroom.location.lon,
                                                    aggregateDiningResult.bathroom.location.lat))
                                            .setAdCode(aggregateDiningResult.bathroom.adcode);
                                    poiList.add(poiInfoEntity);
                                } catch (NumberFormatException e) {
                                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, NUMBER_FORMAT_EXCEPTION + e.getMessage());
                                }
                            });
                } else {
                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "others is null");
                }
                break;
            case AutoMapConstant.AggregateKeywordType.UNKNOWN:
                Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "AggregateKeywordType.UNKNOWN");
                break;
            default:
                break;
        }
        searchResultEntity.setPoiList(poiList);
        return searchResultEntity;
    }

    /**
     * 将顺路关键字搜索结果 SearchEnRouteResult 转换为 SearchResultEntity
     * @param requestParameterBuilder 请求参数
     * @param result 搜索结果
     * @return SearchResultEntity
     */
    public SearchResultEntity mapFromSearchEnRouteResult(final SearchRequestParameter requestParameterBuilder, final SearchEnrouteResult result) {
        final SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, result.errorCode, result.errorMessage);
        final List<PoiInfoEntity> poiList = Optional.ofNullable(result.poiInfos)
                .map(list -> IntStream.range(0, list.size())
                        .mapToObj(index -> {
                            SearchEnroutePoiInfo searchPoi = list.get(index);
                            return convertToPoiInfoEntity(searchPoi, index);
                        })
                        .sorted(Comparator.comparingInt(PoiInfoEntity::getSort_distance))
                        .collect(Collectors.toList()))
                .orElse(Collections.emptyList());
        searchResultEntity.setPoiList(poiList);
        searchResultEntity.setTotal(poiList.size());
        searchResultEntity.setPoiType(Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork()) ? 1 : 0);//0=离线数据，1=在线数据

        //获取筛选分类数据
        if (result.classify != null) {
            searchResultEntity.setRetain(result.classify.retainState);//筛选搜索需要的信息
            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "mapFromSearchEnRouteResult retainState: " + result.classify.retainState);
            if (result.classify.classifyItemInfo != null) {
                //一筛参数列表
                final List<SearchCategoryLocalInfo> categoryLocalInfoList = Optional.ofNullable(result.classify.classifyItemInfo.categoryInfoList)
                        .orElse(new ArrayList<>())
                        .stream()
                        .map(this::mapSearchCategoryInfo)
                        .collect(Collectors.toList());
                if (!categoryLocalInfoList.isEmpty()) {
                    for (int i = 0; i < categoryLocalInfoList.size(); i++) {
                        final List<SearchChildCategoryLocalInfo> level1Infos = Optional.ofNullable(result.classify.classifyItemInfo.categoryInfoList.
                                        get(i).childCategoryInfo)
                                .orElse(new ArrayList<>())
                                .stream()
                                .map(this::mapSearchChildCategoryInfo)
//                                .filter(this::shouldIncludeInResult)//过滤value或者name值不存在的item
                                .collect(Collectors.toList());
                        if (!level1Infos.isEmpty()) {
                            for (int j = 0; j < level1Infos.size(); j++) {
                                final List<SearchChildCategoryLocalInfo> level1ChildInfos = Optional.ofNullable(
                                                result.classify.classifyItemInfo.categoryInfoList.
                                                        get(i).childCategoryInfo.
                                                        get(j).childCategoryInfoList)
                                        .orElse(new ArrayList<>())
                                        .stream()
                                        .map(this::mapSearchChildCategoryInfo)
//                                        .filter(this::shouldIncludeInResult)//过滤value或者name值不存在的item
                                        .collect(Collectors.toList());
                                level1Infos.get(j).setCategoryLocalInfos(level1ChildInfos);
                            }
                            // 过滤掉没有子节点且自身没有value的数据
                            level1Infos.removeIf(info -> !ConvertUtils.isEmpty(info.getCategoryLocalInfos()) && info.getCategoryLocalInfos().isEmpty() && ConvertUtils.isEmpty(info.getValue()));
                        }
                        categoryLocalInfoList.get(i).setCategoryLocalInfos(level1Infos);
                    }
                }
                searchResultEntity.setLocalInfoList(categoryLocalInfoList);
            }

        }
        return searchResultEntity;
    }

    /**
     * 将pid批量搜索结果 SearchEnRouteResult 转换为 SearchResultEntity
     * @param requestParameterBuilder 请求参数
     * @param result 搜索结果
     * @return SearchResultEntity
     */
    public SearchResultEntity mapFromSearchBatchResult(final SearchRequestParameter requestParameterBuilder, final SearchBatchPoiDetailResult result) {
        final SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, result.errorCode, result.errorMessage);
        final List<PoiInfoEntity> poiList = Optional.ofNullable(result.poiInfos)
                .orElse(new ArrayList<>())
                .stream()
                .map(this::convertToPoiInfoEntity)
                .collect(Collectors.toList());
        searchResultEntity.setPoiList(poiList);
        searchResultEntity.setPoiType(Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork()) ? 1 : 0);//0=离线数据，1=在线数据
        return searchResultEntity;
    }

    /**
     * 将POI详情搜索结果 PoiDetailSearchResult 转换为 SearchResultEntity
     * @param requestParameterBuilder 请求参数
     * @param result 搜索结果
     * @return SearchResultEntity
     */
    public SearchResultEntity mapFromSearchPoiDetailSearchResult(final SearchRequestParameter requestParameterBuilder,
                                                                 final PoiDetailSearchResult result) {
        final SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, 0, "");
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "name :" + result.baseInfo.poiInfo.poiInfoBase.name
                + "address :" + result.baseInfo.poiInfo.poiInfoBase.address);
        for (PoiDetailShelfInfo buyInfoList : result.groupBuyInfoList) {
            for (PoiDetailProductInfo productInfo : buyInfoList.productInfoList) {
                Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "productName :" + productInfo.spuName
                        + "productCurrentPrice :" + productInfo.currentPrice
                        + "productOriginalPrice :" + productInfo.originalPrice);
            }
        }
        return searchResultEntity;
    }

    /**
     * 将SearchBatchPoiDetailInfo转换成PoiInfoEntity
     * @param poiInfo SearchBatchPoiDetailInfo
     * @return PoiInfoEntity
     */
    private PoiInfoEntity convertToPoiInfoEntity(final SearchBatchPoiDetailInfo poiInfo) {
        //充电站信息
        final List<ChargeInfo> chargeInfos = new ArrayList<>();
        final int slowFree = poiInfo.chargingStationInfo.slowPileInfo.free;
        final int slowTotal = poiInfo.chargingStationInfo.slowPileInfo.num;
        final int fastFree = poiInfo.chargingStationInfo.fastPileInfo.free;
        final int fastTotal = poiInfo.chargingStationInfo.fastPileInfo.num;
        final ChargeInfo chargeInfo = new ChargeInfo()
                .setSlow_free(slowFree)
                .setSlow_total(slowTotal)
                .setFast_free(fastFree)
                .setFast_total(fastTotal)
                .setCurrentElePrice(String.valueOf(poiInfo.chargingStationInfo.currentPrice.charging))
                .setCurrentServicePrice(poiInfo.chargingStationInfo.parkPrice);
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "slow free: " + poiInfo.chargingStationInfo.slowPileInfo.free
                + " slow total: " + poiInfo.chargingStationInfo.slowPileInfo.num
                + " fast free: " + poiInfo.chargingStationInfo.fastPileInfo.free
                + " fast total: " + poiInfo.chargingStationInfo.fastPileInfo.num
                + " current price: " + poiInfo.chargingStationInfo.currentPrice.charging
                + " address: " + poiInfo.locationInfo.addressInfo.address
                + " name: " + poiInfo.basicInfo.name
                + " childType: " + poiInfo.basicInfo.childType
                + " parkPrice: " + poiInfo.chargingStationInfo.parkPrice);
        chargeInfos.add(chargeInfo);


        //标签信息
        final List<LabelInfo> labelInfos = new ArrayList<>();
        for (SearchLabelInfo searchLabelInfo : poiInfo.labelInfos) {
            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, " label info type: " + searchLabelInfo.type + " ,content: " + searchLabelInfo.content);
            final LabelInfo labelInfo = new LabelInfo()
                    .setMContent(searchLabelInfo.content)
                    .setMType(searchLabelInfo.type);
            labelInfos.add(labelInfo);
        }

        return new PoiInfoEntity()
                .setMChargeChildType(poiInfo.basicInfo.childType)
                .setPointTypeCode(poiInfo.basicInfo.typeCode)
                .setPid(poiInfo.basicInfo.poiId)
                .setName(poiInfo.basicInfo.name)
                .setAddress(poiInfo.locationInfo.addressInfo.address)
                .setPoint(new GeoPoint(poiInfo.basicInfo.location.lon, poiInfo.basicInfo.location.lat))
                .setBusinessTime(poiInfo.basicInfo.openTime)
                .setMLableList(labelInfos)
                .setChargeInfoList(chargeInfos);
    }

    /**
     * 将搜索结果转换为PoiInfoEntity
     * @param poiInfo 搜索结果
     * @return PoiInfoEntity
     */
    private PoiInfoEntity convertToPoiInfoEntity(final SearchEnroutePoiInfo poiInfo, final int index) {
        //加油站信息
        final List<GasStationInfo> gasStationInfos = new ArrayList<>();
        for (int i = 0; i < poiInfo.gasStationInfo.types.size(); i++) {
            gasStationInfos.add(new GasStationInfo()
                    .setType(poiInfo.gasStationInfo.types.get(i))
                    .setPrice(poiInfo.gasStationInfo.prices.get(i)));
            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "gas type is: " + poiInfo.gasStationInfo.types.get(i)
                    + "gas price is: " + poiInfo.gasStationInfo.prices.get(i));
        }
        //充电站信息
        final List<ChargeInfo> chargeInfos = new ArrayList<>();
        final int slowFree = TextUtils.isEmpty(poiInfo.chargingStationInfo.slow_free) ?
                0 : Integer.parseInt(poiInfo.chargingStationInfo.slow_free);
        final int slowTotal = TextUtils.isEmpty(poiInfo.chargingStationInfo.slow_total) ?
                0 : Integer.parseInt(poiInfo.chargingStationInfo.slow_total);
        final int fastFree = TextUtils.isEmpty(poiInfo.chargingStationInfo.fast_free) ?
                0 : Integer.parseInt(poiInfo.chargingStationInfo.fast_free);
        final int fastTotal = TextUtils.isEmpty(poiInfo.chargingStationInfo.fast_total) ?
                0 : Integer.parseInt(poiInfo.chargingStationInfo.fast_total);
        double price = 0;
        // 充电费用：电费
        if(!ConvertUtils.isEmpty(poiInfo.chargingStationInfo.currentPrice)){
            price = poiInfo.chargingStationInfo.currentPrice.charging;
        }
        final ChargeInfo chargeInfo = new ChargeInfo()
                .setSlow_free(slowFree)
                .setSlow_total(slowTotal)
                .setFast_free(fastFree)
                .setFast_total(fastTotal)
                .setMBrand(poiInfo.chargingStationInfo.brand_desc)
                .setCurrentElePrice( price > 0 ? String.valueOf(price) : "0.00")
                .setCurrentServicePrice(poiInfo.chargingStationInfo.parkPrice);
        Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "slow free: " + poiInfo.chargingStationInfo.slow_free
                + " slow total: " + poiInfo.chargingStationInfo.slow_total
                + " fast free: " + poiInfo.chargingStationInfo.fast_free
                + " fast total: " + poiInfo.chargingStationInfo.fast_total
                + " current_ele_price: " + poiInfo.chargingStationInfo.currentPrice.charging
                + " current_service_price: " + poiInfo.chargingStationInfo.currentPrice.service
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
        }
        chargeInfos.add(chargeInfo);

        //停车场信息,对象中不包含，赋默认值
        final List<ParkingInfo> parkingInfoList = new ArrayList<>();
        final ParkingInfo parkingInfo = new ParkingInfo()
                .setSpaceTotal(0)
                .setSpaceFree(0);
        parkingInfoList.add(parkingInfo);
        Logger.e(MapDefaultFinalTag.SEARCH_SERVICE_TAG, " ;name is: " + poiInfo.basicInfo.name
                + "  ;searchPoiInfo.basicInfo.distance:" + poiInfo.basicInfo.distance
                + " ;searchPoiInfo.basicInfo.tag:" + poiInfo.basicInfo.tag);
        //标签信息
        final List<LabelInfo> labelInfos = new ArrayList<>();
        for (SearchLabelInfo searchLabelInfo : poiInfo.labelInfo) {
            if(BuildConfig.DEBUG){
                Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, " name is: " + poiInfo.basicInfo.name + " label info type: " + searchLabelInfo.type + " ,subType: " + searchLabelInfo.subType + " ,content: " + searchLabelInfo.content);
            }
            final LabelInfo labelInfo = new LabelInfo()
                    .setMContent(searchLabelInfo.content)
                    .setMType(searchLabelInfo.type)
                    .setMSubType(searchLabelInfo.subType);
            labelInfos.add(labelInfo);
        }

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
                .setMLableList(labelInfos)
                .setPoiTag(isParking(poiInfo.basicInfo.typeCode) ? "停车场" : poiInfo.basicInfo.tag)
                .setSort_distance(ConvertUtils.str2Int(poiInfo.basicInfo.distance))
                .setMIndex(index)
                .setChargeInfoList(chargeInfos);
    }

    /**
     * 将深度信息 搜索结果 SearchDeepInfoResult 转换为 SearchResultEntity
     * @param requestParameterBuilder 请求参数
     * @param result 搜索结果
     * @return SearchResultEntity
     */
    public SearchResultEntity mapFromDeepInfoSearchResult(final SearchRequestParameter requestParameterBuilder, final SearchDeepInfoResult result) {
        final SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, result.code, result.message);
        final List<PoiInfoEntity> poiList = Optional.ofNullable(result.deepinfoPoi)
                .map(deepinfoPoi -> {
                    final PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                            .setPid(deepinfoPoi.poiid)
                            .setName(deepinfoPoi.name)
                            .setAddress(deepinfoPoi.address)
                            .setPoint(new GeoPoint(deepinfoPoi.poi_loc.lon, deepinfoPoi.poi_loc.lat))
                            .setAdCode(deepinfoPoi.adcode)
                            .setPhone(deepinfoPoi.tel)
                            .setBusinessTime(deepinfoPoi.opentime);

                    final List<GasStationInfo> stationList = Optional.ofNullable(deepinfoPoi.gasinfoList)
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
     * @param requestParameterBuilder 请求参数
     * @param result 搜索结果
     * @return SearchResultEntity
     */
    public SearchResultEntity mapFromLineDeepInfoSearchResult(final SearchRequestParameter requestParameterBuilder,
                                                              final SearchLineDeepInfoResult result) {
        final SearchResultEntity searchResultEntity = createBaseResultEntity(requestParameterBuilder, result.code, result.message);

        final List<ServiceAreaInfo> serviceAreaInfoList = new ArrayList<>();
        final List<ParkingInfo> parkingInfoList = new ArrayList<>();
        final List<ChargeInfo> chargeInfoList = new ArrayList<>();
        final List<GasStationInfo> stationList = new ArrayList<>();

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
                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "未知错误类型");
                    break;
                default:
                    Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "未处理的类型: " + linePoiBase.queryType);
                    break;
            }
        });

        final PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                .setServiceAreaInfoList(serviceAreaInfoList)
                .setChargeInfoList(chargeInfoList)
                .setStationList(stationList)
                .setParkingInfoList(parkingInfoList);

        searchResultEntity.setPoiList(List.of(poiInfoEntity));
        return searchResultEntity;
    }

    /**
     * 将沿途搜索服务区结果LinePoiServiceAreaInfo转换为ServiceAreaInfo
     * @param linePoiServiceAreaInfo 沿途搜索服务区结果
     * @return ServiceAreaInfo
     */
    private ServiceAreaInfo mapServiceAreaInfo(final LinePoiServiceAreaInfo linePoiServiceAreaInfo) {
        final List<ServiceAreaInfo.ServiceAreaChild> serviceAreaChildList = linePoiServiceAreaInfo.children.stream()
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

    /**
     * 将沿途搜索服务区子节点LinePoiServiceAreaChild转换为ServiceAreaChild
     * @param linePoiServiceAreaChild 沿途搜索服务区子节点
     * @return ServiceAreaChild
     */
    private ServiceAreaInfo.ServiceAreaChild mapServiceAreaChild(final LinePoiServiceAreaChild linePoiServiceAreaChild) {
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

    /**
     * 将沿途搜索计费信息LinePoiChargeInfo转换为ChargeInfo
     * @param linePoiChargeInfo 沿途搜索计费信息
     * @return ChargeInfo
     */
    private ChargeInfo mapChargeInfo(final LinePoiChargeInfo linePoiChargeInfo) {
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
                .setOpen24h(linePoiChargeInfo.open24h)
                .setOpenTime(linePoiChargeInfo.openTime)
                .setSuperTotal(linePoiChargeInfo.superTotal);
    }

    /**
     * 将沿途搜索加油站信息LinePoiGasStationInfo转换为GasStationInfo
     * @param linePoiGasStationInfo 沿途搜索加油站信息
     * @return GasStationInfo
     */
    private GasStationInfo mapGasStationInfo(final LinePoiGasStationInfo linePoiGasStationInfo) {
        return new GasStationInfo()
                .setQueryType(linePoiGasStationInfo.queryType)
                .setPoiId(linePoiGasStationInfo.poiId)
                .setName(linePoiGasStationInfo.name)
                .setTypeCode(linePoiGasStationInfo.typecode)
                .setPrice("7.9")
                .setDiscount(linePoiGasStationInfo.discount);
    }

    /**
     * 将沿途搜索停车场信息LinePoiParkRecommendInfo转换为ParkingInfo
     * @param linePoiParkRecommendInfo 沿途搜索停车场信息
     * @return ParkingInfo
     */
    private ParkingInfo mapParkingInfo(final LinePoiParkRecommendInfo linePoiParkRecommendInfo) {
        return new ParkingInfo()
                .setQueryType(linePoiParkRecommendInfo.queryType)
                .setPoiId(linePoiParkRecommendInfo.poiId)
                .setName(linePoiParkRecommendInfo.name)
                .setTypeCode(linePoiParkRecommendInfo.typecode)
                .setAddress(linePoiParkRecommendInfo.address);
    }


    /**
     * 创建基础的 SearchResultEntity
     *
     * @param requestParameterBuilder 请求参数构建器
     * @param code 错误码
     * @param message 消息
     * @return SearchResultEntity
     */
    private SearchResultEntity createBaseResultEntity(final SearchRequestParameter requestParameterBuilder, final int code, final String message) {
        return new SearchResultEntity()
                .setCode(code)
                .setMessage(message)
                .setKeyword(requestParameterBuilder.getKeyword())
                .setPageNum(requestParameterBuilder.getPage())
                .setSearchType(requestParameterBuilder.getSearchType());
    }

    /**
     * 格式化距离数组
     * @param distance 距离
     * @return String
     */
    private String formatDistanceArrayInternal(final int distance) {
        final String[] distanceArray = ConvertUtils.formatEnDistanceArray(AppCache.getInstance().getMContext(), distance);
        return distanceArray[0] + distanceArray[1];
    }

    /**
     * 判断是否是停车场
     * @param typeCode 类型编码
     * @return boolean
     */
    private boolean isParking(final String typeCode) {
        /* 停车场类型
         * 150903
         * 150904
         * 150905
         * 150906
         * 150907
         * 150908
         * 150909
         */
        final String regex = "^150900|15090[3-9]$";
        return Pattern.matches(regex, typeCode);
    }
}