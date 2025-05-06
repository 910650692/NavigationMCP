package com.fy.navi.hmi.favorite;

import android.app.Application;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;

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
}
