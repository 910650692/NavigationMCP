package com.sgm.navi.hmi.favorite;

import android.app.Application;

import androidx.annotation.NonNull;

import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;

public class BaseHomeCompanyViewModel extends BaseViewModel<HomeCompanyFragment, HomeCompanyModel>  {

    public BaseHomeCompanyViewModel(final @NonNull Application application) {
        super(application);
    }
    private int mHomeCompanyType;

    @Override
    protected HomeCompanyModel initModel() {
        return new HomeCompanyModel();
    }

    public Action mRootClick = () -> {
    };

    /**
     * setHomeCompanyType
     * @param homeCompanyType 1:家 2:公司 3:常用地址 0:收藏夹
     */
    public void setHomeCompanyType(final int homeCompanyType) {
        this.mHomeCompanyType = homeCompanyType;
    }

    /**
     * notifySearchResult
     * @param taskId 任务id
     * @param searchResultEntity 数据回调实体类
     */
    public void notifySearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        mView.notifySearchResult(taskId, searchResultEntity);
    }

}
