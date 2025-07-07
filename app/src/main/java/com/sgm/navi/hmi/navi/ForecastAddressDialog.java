package com.sgm.navi.hmi.navi;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.hmi.BuildConfig;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.DialogForecastAddressBinding;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.define.user.forecast.OftenArrivedItemInfo;
import com.sgm.navi.service.logicpaket.user.forecast.IForecastAddressCallBack;
import com.sgm.navi.ui.dialog.BaseFullScreenDialog;


public class ForecastAddressDialog extends BaseFullScreenDialog<DialogForecastAddressBinding> {

    private IForecastAddressCallBack mIForecastAddressCallBack;
    private int mType;
    private OftenArrivedItemInfo mOftenArrivedItemInfo;

    public ForecastAddressDialog(Context context, final IForecastAddressCallBack addressCallBack){
        super(context);
        this.mIForecastAddressCallBack = addressCallBack;
    }

    public void setForecastAddressInfo(final int type, final OftenArrivedItemInfo oftenArrivedItemInfo){
        this.mType = type;
        this.mOftenArrivedItemInfo = oftenArrivedItemInfo;
        updateForecastAddressInfo();
    }

    @Override
    protected DialogForecastAddressBinding initLayout() {
        return DialogForecastAddressBinding.inflate(LayoutInflater.from(getContext()));
    }

    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setDialogView();
    }

    private void setDialogView() {
        //todo flavor temp
        if (BuildConfig.FLAVOR.equals("clea_8255")
                || BuildConfig.FLAVOR.equals("clea_local_8155")
                || BuildConfig.FLAVOR.equals("clea_8775")) {
            mViewBinding.forecastContent.setCompoundDrawablesWithIntrinsicBounds(null, null, null, null);
            mViewBinding.forecastEdit.setVisibility(View.VISIBLE);
        }
    }

    private void updateForecastAddressInfo(){
        if(!ConvertUtils.isEmpty(mOftenArrivedItemInfo.getWstrAddress())){
            mViewBinding.forecastContent.setText(mOftenArrivedItemInfo.getWstrAddress());
        }
        if(mType == AutoMapConstant.HomeCompanyType.HOME){
            mViewBinding.forecastTitle.setText(ResourceUtils.Companion.getInstance().getText(R.string.forecast_title_home));
        }else {
            mViewBinding.forecastTitle.setText(ResourceUtils.Companion.getInstance().getText(R.string.forecast_title_company));
        }

        mViewBinding.dialogCommit.setOnClickListener(v -> {
            onDismiss();
            //设置数据
            mIForecastAddressCallBack.AddForecastInfo(mType, mOftenArrivedItemInfo);

            if(mViewBinding.forecastTitle.getText() == ResourceUtils.Companion.getInstance().getText(R.string.forecast_title_home)){
                sendBuryPointForPrediction(BuryConstant.Number.ONE);
            }
        });

        mViewBinding.dialogCancel.setOnClickListener(v -> {
            onDismiss();

            if(mViewBinding.forecastTitle.getText() == ResourceUtils.Companion.getInstance().getText(R.string.forecast_title_home)){
                sendBuryPointForPrediction(BuryConstant.Number.SECOND);
            }
        });

        mViewBinding.forecastContent.setOnClickListener(v -> {
            onDismiss();
            mIForecastAddressCallBack.addressClick(mType);
        });

        mViewBinding.forecastEdit.setOnClickListener(v -> {
            onDismiss();
            mViewBinding.forecastContent.callOnClick();
        });
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_HOME_PREDICTION)
    private void sendBuryPointForPrediction(String number){
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION, number)
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }

    /**
     * dismiss dialog
     */
    private void onDismiss(){
        dismiss();
    }
}
