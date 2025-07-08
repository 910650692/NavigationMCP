package com.sgm.navi.hmi.favorite;

import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.view.View;
import android.view.WindowManager;

import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentFavoriteRenameBinding;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.sgm.navi.ui.base.BaseFragment;

public class FavoriteRenameFragment extends BaseFragment<FragmentFavoriteRenameBinding, FavoriteRenameViewModel> {

    private String mName = "";
    @Override
    public int onLayoutId() {
        return R.layout.fragment_favorite_rename;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        mBinding.settingFavoriteRename.setOnFocusChangeListener((v, hasFocus) -> {
            if (hasFocus) {
                showSoftKeyboard();
            }
        });
        mBinding.settingFavoriteRename.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {}

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {}

            @Override
            public void afterTextChanged(Editable s) {
                mViewModel.mCloseViewVisibility.setValue(s.toString().trim().isEmpty() ? true : false);
            }
        });
    }

    @Override
    public void onInitData() {
        final Bundle bundle = getArguments();
        if (bundle != null) {
            final PoiInfoEntity poiInfoEntity = bundle.getParcelable("rename");
            if (poiInfoEntity != null) {
                String customName = "";
                if (poiInfoEntity.getFavoriteInfo() != null) {
                    customName = poiInfoEntity.getFavoriteInfo().getCustom_name();
                    mName = poiInfoEntity.getName();
                }
                setEditTextContent(TextUtils.isEmpty(customName) ? poiInfoEntity.getName() : customName);
            }
        }
    }

    /**
     * setEditTextContent
     * @param content
     */
    public void setEditTextContent(String content) {
        mBinding.settingFavoriteRename.requestFocus();
        //mBinding.settingFavoriteRename.setText(content); //UE确认，此处要置空，用户重新输入
        //mBinding.settingFavoriteRename.setSelection(content.length()); //此处光标不需要设置了
    }

    /**
     * clearEditText
     */
    public void clearEditText() {
        mBinding.settingFavoriteRename.setText("");
    }

    /**
     * renameFinished
     */
    public void renameFinished() {
        String newName = mBinding.settingFavoriteRename.getText().toString().trim();
        SettingUpdateObservable.getInstance().onUpdateRename(TextUtils.isEmpty(newName) ? mName : newName);
    }

    /**
     * 显示系统键盘
     */
    private void showSoftKeyboard() {
        if (getActivity() == null || getActivity().getWindow() == null) {
            return;
        }
        getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_VISIBLE);
    }
}
