package com.fy.navi.hmi.drivingrecord.recordlogin;

import android.graphics.Bitmap;
import android.view.View;

import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentDrivingRecordLoginBinding;
import com.fy.navi.service.define.user.account.QRCodeType;
import com.fy.navi.ui.base.BaseFragment;

/**
 * @Description 用户行驶里程
 * @Author fh
 * @date 2024/12/24
 */
public class DrivingRecordLoginFragment extends BaseFragment<FragmentDrivingRecordLoginBinding, DrivingRecordLoginViewModel> {

    @Override
    public int onLayoutId() {
        return R.layout.fragment_driving_record_login;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        mViewModel.qRCodeLoginRequest(QRCodeType.QRCodeTypeDefault);
    }

    @Override
    public void onInitData() {
        ThreadManager.getInstance().postDelay(() -> {
            mViewModel.getValueByType();
        },0);
    }

    @Override
    public void onResume() {
        super.onResume();
    }

    @Override
    public void onInitObserver() {
        super.onInitObserver();
    }


    public void updateQRCode(Bitmap bitmap) {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.codeImg.setImageBitmap(bitmap);
            mBinding.qrcodeImg.setImageBitmap(bitmap);

        });
    }

    public void updateNoDataView(int size) {
        ThreadManager.getInstance().postUi(() -> {
            // 未登录高德账号时，需判断当前是否存在行程历史数据
            if (size > 0) { // 未登录存在历史数据
                mBinding.dataLoginView.setVisibility(View.VISIBLE);
                mBinding.noDataLoginView.setVisibility(View.GONE);
            } else {
                mBinding.dataLoginView.setVisibility(View.GONE);
                mBinding.noDataLoginView.setVisibility(View.VISIBLE);
            }
        });
    }




}
