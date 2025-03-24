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
}
