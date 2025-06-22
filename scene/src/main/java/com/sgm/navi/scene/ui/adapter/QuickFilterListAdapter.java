package com.sgm.navi.scene.ui.adapter;

import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.QuickFilterItemBinding;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.search.SearchChildCategoryLocalInfo;

import java.util.ArrayList;
import java.util.List;

public class QuickFilterListAdapter extends RecyclerView.Adapter<QuickFilterListAdapter.Holder>{
    private final List<SearchChildCategoryLocalInfo> mSearchCategoryLocalInfos;
    private OnItemClickListener mItemClickListener;
    @NonNull
    @Override
    public Holder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        final QuickFilterItemBinding quickFilterItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.quick_filter_item, parent, false);
        return new QuickFilterListAdapter.Holder(quickFilterItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull Holder holder, int position) {
        if(position > mSearchCategoryLocalInfos.size()) return;
        SearchChildCategoryLocalInfo searchCategoryLocalInfo = mSearchCategoryLocalInfos.get(position);
        if(ConvertUtils.isNull(searchCategoryLocalInfo)){
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onBindViewHolder localInfo is null");
            return;
        }
        holder.binding.filterText.setText(searchCategoryLocalInfo.getName());
        holder.binding.filterRoot.setSelected(searchCategoryLocalInfo.getChecked() == 1);
        holder.binding.getRoot().setOnClickListener(v -> {
            for (int i = 0; i < mSearchCategoryLocalInfos.size(); i++) {
                mSearchCategoryLocalInfos.get(i).setChecked(i == position ? 1 : 0);
            }
            notifyDataSetChanged();
            mItemClickListener.onItemClick(mSearchCategoryLocalInfos,position);
        });
    }

    @Override
    public int getItemCount() {
        return mSearchCategoryLocalInfos.size();
    }

    public QuickFilterListAdapter() {
        mSearchCategoryLocalInfos = new ArrayList<>();
    }

    /**
     * 设置label列表
     * @param infos 分类列表
     */
    public void setLabelList(final List<SearchChildCategoryLocalInfo> infos) {
        if (ConvertUtils.isEmpty(infos)) {
            return;
        }

        mSearchCategoryLocalInfos.clear();
        mSearchCategoryLocalInfos.addAll(infos);
        notifyDataSetChanged();
    }

    public void setItemClickListener(OnItemClickListener itemClickListener){
        this.mItemClickListener = itemClickListener;
    }

    public static class Holder extends RecyclerView.ViewHolder {
        private QuickFilterItemBinding binding;

        public Holder(@NonNull QuickFilterItemBinding itemView) {
            super(itemView.getRoot());
            this.binding = itemView;
        }
    }

    public interface OnItemClickListener {
        default void onItemClick(List<SearchChildCategoryLocalInfo> list,int position){}
    }
}
