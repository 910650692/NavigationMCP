package com.fy.navi.hmi.search.searchresult;


import android.app.Application;
import android.os.Bundle;
import android.util.Log;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonSerializer;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;


public class BaseSearchResultViewModel extends BaseViewModel<SearchResultFragment, SearchResultModel> {
    public BaseSearchResultViewModel(@NonNull final Application application) {
        super(application);
    }

    @Override
    protected SearchResultModel initModel() {
        return new SearchResultModel();
    }

    private final Action mRootClick = () -> {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "rootClick: ");
    };

    public Action getRootClick() {
        return mRootClick;
    }

    /**
     * 搜索结果
     * @param taskId 请求id
     * @param searchResultEntity 搜索结果实体类
     */
    public void notifySearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        mView.notifySearchResult(taskId, searchResultEntity);
    }

    /**
     * 路线变化回调
     */
    public void onRouteSelected() {
        mView.onRouteSelected();
    }

    /**
     * 静默搜索回调
     * @param searchResultEntity 搜索结果实体类
     * @param taskId 任务id
     */
    public void notifySilentSearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        mView.notifySilentSearchResult(taskId, searchResultEntity);
    }

    /**
     * 语音筛选排序回调
     * @param sortValue 排序条件
     */
    public void onVoicePoiSort(final String sortValue) {
        mView.onVoicePoiSort(sortValue);
    }

    /**
     * 图层点击事件回调
     * @param index 点击下标
     */
    public void onMarkClickCallBack(final int index) {
        mView.onMarkClickCallBack(index);
    }

    /**
     * 恢复fragment状态
     */
    public void onReStoreFragment() {
        mModel.onReStoreFragment();
    }


    /**
     * 搜索结果列表页面可变状态变化回调
     * @param isShow 是否可见
     */
    public void updateShowState(final boolean isShow) {
        mModel.updateShowState(isShow);
    }

    /**
     * 日夜模式切换保存数据到model
     * @param taskId 请求数据的taskId
     * @param searchResultEntity SearchResultEntity对象
     */
    public void saveData(final int taskId, final SearchResultEntity searchResultEntity) {
        mModel.saveData(taskId, searchResultEntity);
    }

    /**
     * 注册路线变化回调
     */
    public void registerRouteCallback() {
        mModel.registerRouteCallback();
    }

    public void notifyNetSearchResult(int taskId,BaseRep result){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"code: "+result.getResultCode());
        if(AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())) {
            if(!AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())){
                mView.notifySearchResultByNetError(result.getResultCode());
                return;
            }
            ArrayList<PoiInfoEntity> list = new ArrayList<>();
            try {
                JSONObject jsonObject = new JSONObject(GsonUtils.toJson(result.getDataSet()));
                JSONArray jsonArray = jsonObject.getJSONArray("resultList");
                if(jsonArray.length() > 0){
                    for (int i = 0; i < jsonArray.length(); i++) {
                        PoiInfoEntity entity = GsonUtils.fromJson(jsonArray.getString(i),PoiInfoEntity.class);
                        JSONObject object = new JSONObject(String.valueOf(jsonArray.get(i)));
                        GeoPoint point = new GeoPoint();
                        point.setLat(ConvertUtils.str2Double(object.getString("stationLat")));
                        point.setLon(ConvertUtils.str2Double(object.getString("stationLng")));
                        entity.setPoint(point);
                        if(!ConvertUtils.isEmpty(object.getDouble("distance"))){
                            int distance = ConvertUtils.double2int(object.getDouble("distance") * 1000);
                            final String[] distanceArray = ConvertUtils.formatDistanceArray(getApplication().getBaseContext(),distance);
                            entity.setDistance(distanceArray[0]+distanceArray[1]);
                        }
                        ChargeInfo chargeInfo = GsonUtils.fromJson(jsonArray.getString(i),ChargeInfo.class);
                        chargeInfo.setCurrentElePrice(chargeInfo.getLowPrice())
                                .setFast_free(chargeInfo.getFastChargingFree())
                                .setFast_total(chargeInfo.getFastChargingTotal())
                                .setSlow_free(chargeInfo.getSlowChargingFree())
                                .setSlow_total(chargeInfo.getSlowChargingTotal());
                        List<ChargeInfo> chargeList = new ArrayList<>();

                        chargeList.add(chargeInfo);
                        entity.setChargeInfoList(chargeList)
                                .setName(entity.getStationName())
                                .setAddress(entity.getStationAddress())
                                .setPhone(entity.getStationTel())
                                .setPointTypeCode("011100")
                                .setBusinessTime(entity.getStationBusinessTime());
                        list.add(entity);
                    }
                }
                SearchResultEntity searchResultEntity = new SearchResultEntity()
                        .setIsNetData(true)
                        .setPoiList(list)
                        .setPoiType(1);
                mModel.addPoiMarker(searchResultEntity);
                mView.notifySearchResultByNet(taskId,searchResultEntity);
            }catch (JSONException e){
                Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG,"error: "+e);
                mView.notifySearchResultByNetError("idle");
            }
        }else{
            mView.notifySearchResultByNetError("idle");
        }
    }
}
