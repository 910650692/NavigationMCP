package com.fy.navi.hmi.search.searchresult;


import android.app.Application;
import android.os.Bundle;
import android.util.Log;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.define.bean.GeoPoint;
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
     * 静默搜索回调
     * @param searchResultEntity 搜索结果实体类
     */
    public void notifySilentSearchResult(final SearchResultEntity searchResultEntity) {
        mView.notifySilentSearchResult(searchResultEntity);
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

    public void notifyNetSearchResult(int taskId,BaseRep result){
        if(!ConvertUtils.isNull(result)){
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"code: "+result.getResultCode());
            ArrayList<PoiInfoEntity> list = new ArrayList<>();
            try {
                JSONObject jsonObject = new JSONObject(String.valueOf(result.getDataSet()));
                JSONArray jsonArray = jsonObject.getJSONArray("resultList");
                if(jsonArray.length() > 0){
                    for (int i = 0; i < jsonArray.length(); i++) {
                        PoiInfoEntity entity = GsonUtils.fromJson(jsonArray.getString(i),PoiInfoEntity.class);
                        JSONObject object = new JSONObject(String.valueOf(jsonArray.get(i)));
                        GeoPoint point = new GeoPoint();
                        point.setLat(ConvertUtils.str2Double(object.getString("stationLat")));
                        point.setLon(ConvertUtils.str2Double(object.getString("stationLng")));
                        entity.setPoint(point);
                        list.add(entity);
                    }
                }else{
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"resultList is empty");
                    return;
                }
                SearchResultEntity searchResultEntity = new SearchResultEntity()
                        .setIsNetData(true)
                        .setPoiList(list)
                        .setPoiType(1);
                mView.notifySearchResultByNet(taskId,searchResultEntity);
            }catch (JSONException e){
                Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG,"error: "+e);
            }
        }
    }
}
