package com.sgm.navi.hmi.startup;

import android.content.Context;
import android.os.Bundle;

import com.android.utils.NetWorkUtils;
import com.sgm.navi.hmi.databinding.DialogStartupExceptionBinding;
import com.sgm.navi.ui.dialog.BaseFullScreenDialog;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

public class StartupExceptionDialog extends BaseFullScreenDialog<DialogStartupExceptionBinding> {
    private NetWorkUtils.NetworkObserver networkCall;

    public StartupExceptionDialog(Context context, IBaseDialogClickListener baseDialogClickListener) {
        super(context);
        setDialogClickListener(baseDialogClickListener);
    }

    @Override
    protected DialogStartupExceptionBinding initLayout() {
        return DialogStartupExceptionBinding.inflate(getLayoutInflater());
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        networkCall = new NetWorkUtils.NetworkObserver() {
            @Override
            public void onNetConnectSuccess() {
                if (mDialogClickListener != null) {
                    mDialogClickListener.onNetWorkConnect();
                }
            }

            @Override
            public void onNetDisConnect() {

            }

            @Override
            public void onNetLinkPropertiesChanged() {

            }

            @Override
            public void onNetLosing() {

            }

            @Override
            public void onNetBlockedStatusChanged() {

            }

            @Override
            public void onNetUnavailable() {

            }


        };
        NetWorkUtils.Companion.getInstance().registerNetworkObserver(networkCall);
        mViewBinding.tvExit.setOnClickListener(v -> {
            if (mDialogClickListener != null) {
                mDialogClickListener.onExit();
            }
        });
    }

    @Override
    public void onDetachedFromWindow() {
        super.onDetachedFromWindow();
        NetWorkUtils.Companion.getInstance().unRegisterNetworkObserver(networkCall);
        setDialogClickListener(null);
    }
}
