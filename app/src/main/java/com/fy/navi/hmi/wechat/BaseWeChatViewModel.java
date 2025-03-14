package com.fy.navi.hmi.wechat;

import android.app.Application;
import android.graphics.Bitmap;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;


public class BaseWeChatViewModel extends BaseViewModel<WeChatFragment, WeChatModel> {



    public MutableLiveData<Boolean> isBind = new MutableLiveData<>(false);

    public BaseWeChatViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected WeChatModel initModel() {
        return new WeChatModel();
    }

    //返回上一页
    public Action chatConnectBack = () -> {
        closeFragment(true);
    };

    public Action howToBind = () -> {
        mView.showUnbindDialog();
    };

    public void setIsBind(boolean isBind) {
        this.isBind.postValue(isBind);
        mView.updateTitle(isBind);
    }


    public void initView() {
        mModel.getBindStatus();
    }

    public void updateQRCode(Bitmap bitmap) {
        mView.updateQRCode(bitmap);
    }

}
