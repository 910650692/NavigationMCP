package com.fy.navi.hmi.account;

import android.text.Editable;
import android.text.TextWatcher;

import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentPhoneNumberLoginBinding;
import com.fy.navi.ui.base.BaseFragment;

public class AccountPhoneNumberLoginFragment extends BaseFragment<FragmentPhoneNumberLoginBinding, AccountPhoneNumberLoginViewModel> {

    @Override
    public int onLayoutId() {
        return R.layout.fragment_phone_number_login;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        initView();
    }

    @Override
    public void onInitData() {

    }

    /**
     * 初始化视图
     */
    private void initView() {
        mBinding.phoneNumberEditText.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(final CharSequence s, final int start, final int count, final int after) {

            }

            @Override
            public void onTextChanged(final CharSequence s, final int start, final int before, final int count) {

            }

            @Override
            public void afterTextChanged(final Editable s) {
                mViewModel.setPhoneNumber(s.toString());
            }
        });

        mBinding.verificationCodeEditText.addTextChangedListener(new TextWatcher() {

            @Override
            public void beforeTextChanged(final CharSequence s, final int start, final int count, final int after) {

            }

            @Override
            public void onTextChanged(final CharSequence s, final int start, final int before, final int count) {

            }

            @Override
            public void afterTextChanged(final Editable s) {
                mViewModel.setVerificationCode(s.toString());
            }
        });
    }
}
