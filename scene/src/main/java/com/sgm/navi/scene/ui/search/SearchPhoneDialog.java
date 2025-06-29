package com.sgm.navi.scene.ui.search;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.LayoutPhoneDialogBinding;
import com.sgm.navi.scene.databinding.SearchPhoneItemBinding;
import com.sgm.navi.scene.ui.adapter.ChargeEquipmentListAdapter;
import com.sgm.navi.service.define.search.ConnectorInfoItem;
import com.sgm.navi.service.define.search.EquipmentInfo;
import com.sgm.navi.ui.dialog.BaseFullScreenDialog;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

import java.util.ArrayList;

import lombok.Setter;

public class SearchPhoneDialog extends BaseFullScreenDialog<LayoutPhoneDialogBinding> {
    private final String mTitle;
    private final ArrayList<String> mPhoneList;
    private final String mConfirmTitle;
    private final IBaseDialogClickListener mDialogClickListener;
    protected SearchPhoneDialog(final Context context, final String title, final String confirmTitle, final ArrayList<String> phoneList, final IBaseDialogClickListener observer) {
        super(context);
        mTitle = title;
        mConfirmTitle = confirmTitle;
        mPhoneList = phoneList;
        mDialogClickListener = observer;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewBinding.lpTitle.setText(mTitle);
        mViewBinding.lpConfim.setText(mConfirmTitle);
        mViewBinding.lpConfim.setOnClickListener(v -> {
            dismiss();
            if (null != mDialogClickListener) {
                mDialogClickListener.onCancelClick();
            }
        });
        setAdapter();
    }

    @Override
    protected LayoutPhoneDialogBinding initLayout() {
        return LayoutPhoneDialogBinding.inflate(LayoutInflater.from(getContext()));
    }

    private void setAdapter(){
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.lpPhoneList.setLayoutManager(layoutManager);
        PhoneListAdapter adapter = new PhoneListAdapter();
        mViewBinding.lpPhoneList.setAdapter(adapter);
        adapter.setItemClickListener(new PhoneListAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(String phone) {
                if (null != mDialogClickListener) {
                    mDialogClickListener.onCommitClick(phone);
                }
            }
        });
        adapter.setPhoneList(mPhoneList);
    }

    public static class Build {
        private final Context mContext;
        private String mTitle;
        private String mConfirmTitle;
        private final ArrayList<String> mPhoneList;
        private IBaseDialogClickListener mDialogObserver;

        public Build(final Context context) {
            this.mContext = context;
            this.mPhoneList = new ArrayList<>();
        }

        /**
         * 设置标题
         * @param title 标题文本
         * @return Build
         */
        public Build setTitle(final String title) {
            this.mTitle = title;
            return this;
        }

        /**
         * 设置确认按钮标题文本
         * @param confirmTitle  确认按钮标题文本
         * @return Build
         */
        public Build setConfirmTitle(final String confirmTitle) {
            this.mConfirmTitle = confirmTitle;
            return this;
        }

        /**
         * 设置内容
         * @param phoneList 电话列表
         * @return Build
         */
        public Build setContent(final ArrayList<String> phoneList) {
            this.mPhoneList.clear();
            this.mPhoneList.addAll(phoneList);
            return this;
        }

        /**
         * 设置点击事件监听
         * @param dialogObserver 点击事件监听
         * @return Build
         */
        public Build setDialogObserver(final IBaseDialogClickListener dialogObserver) {
            this.mDialogObserver = dialogObserver;
            return this;
        }

        /**
         * 构造方法
         * @return SearchConfirmDialog对象
         */
        public SearchPhoneDialog build() {
            return new SearchPhoneDialog(mContext, mTitle, mConfirmTitle,mPhoneList, mDialogObserver);
        }
    }

    public class PhoneListAdapter extends RecyclerView.Adapter<PhoneListAdapter.Holder>{
        private ArrayList<String> mPhoneList;
        @Setter
        private OnItemClickListener mItemClickListener;
        @NonNull
        @Override
        public PhoneListAdapter.Holder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
            final SearchPhoneItemBinding SearchPhoneItemBinding = DataBindingUtil.inflate(LayoutInflater.from(getContext()), R.layout.search_phone_item,parent,false);
            return new Holder(SearchPhoneItemBinding);
        }

        @Override
        public void onBindViewHolder(@NonNull PhoneListAdapter.Holder holder, int position) {
            if(position > mPhoneList.size()) return;
            holder.mBinding.setPhoneText(mPhoneList.get(position));
            holder.mBinding.setHandler(new MyEventHandle());
        }

        @Override
        public int getItemCount() {
            return mPhoneList.size();
        }

        public void setPhoneList(ArrayList<String> list){
            mPhoneList = new ArrayList<>();
            mPhoneList.addAll(list);
            notifyDataSetChanged();
        }

        public void setItemClickListener(OnItemClickListener itemClickListener){
            this.mItemClickListener = itemClickListener;
        }

        public static class Holder extends RecyclerView.ViewHolder{
            private final SearchPhoneItemBinding mBinding;
            public Holder(@NonNull SearchPhoneItemBinding binding) {
                super(binding.getRoot());
                mBinding = binding;
            }
        }

        public class MyEventHandle {
            public void toPhone(String phone){
                if(!ConvertUtils.isEmpty(mItemClickListener)){
                    mItemClickListener.onItemClick(phone);
                }
            }
        }

        public interface OnItemClickListener {
            void onItemClick(String phone);
        }
    }
}
