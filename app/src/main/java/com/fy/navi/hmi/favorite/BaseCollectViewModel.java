package com.fy.navi.hmi.favorite;

import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

public class BaseCollectViewModel extends BaseViewModel<CollectFragment, CollectModel>  {
    public BaseCollectViewModel(final @NonNull Application application) {
        super(application);
    }
    @Override
    protected CollectModel initModel() {
        return new CollectModel();
    }

    public Action rootClick = () -> {
    };

    /**
     * setCollectData
     */
    public void setCollectData() {
        mView.setAdapterData();
    }

    public ArrayList<PoiInfoEntity> getFavoriteListAsync() {
        return mModel.getFavoriteListAsync();
    }

    public List<PoiInfoEntity> getPushMsgList() {
        return mModel.getPushMsgList();
    }

    /**
     * 动力类型标定
     * -1 无效值
     * 0 汽油车
     * 1 纯电动车
     * 2 插电式混动汽车
     * @return 动力类型
     */
    public int powerType() {
        return mModel.powerType();
    }

    public void notifyNetSearchResult(BaseRep result){
        if(!ConvertUtils.isNull(result)){
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"result"+result.getDataSet());
            ArrayList<PoiInfoEntity> list = new ArrayList<>();
            // 回调出的数据转换List
            try {
                JSONObject jsonObject = new JSONObject(String.valueOf(result.getDataSet()));
                JSONArray jsonArray = jsonObject.getJSONArray("items");
                if(jsonArray.length() > 0){
                    for (int i = 0; i < jsonArray.length(); i++) {
                        JSONObject object = new JSONObject(String.valueOf(jsonArray.get(i)));
                        if(object.getBoolean("stationSaved")){
                            GeoPoint point = new GeoPoint();
                            point.setLat(ConvertUtils.str2Double(object.getString("stationLat")));
                            point.setLon(ConvertUtils.str2Double(object.getString("stationLng")));
                            PoiInfoEntity entity = new PoiInfoEntity()
                                    .setName(object.getString("stationName"))
                                    .setAddress(object.getString("address"))
                                    .setOperatorId(object.getString("operatorId"))
                                    .setStationId(object.getString("stationId"))
                                    .setPoint(point);
                            list.add(entity);
                        }
                    }
                }
                mView.setAdapterDataByNet(list);
            } catch (JSONException e) {
                throw new RuntimeException("转换JSONObject失败", e);
            }
        }
    }
}
