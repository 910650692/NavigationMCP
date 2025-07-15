package com.sgm.navi.scene.ui.adapter;

import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.ResultFilterLabelBinding;
import com.sgm.navi.service.define.search.SearchChildCategoryLocalInfo;

import java.util.ArrayList;
import java.util.List;

public class SearchResultFilterLabelAdapter extends RecyclerView.Adapter<SearchResultFilterLabelAdapter.Holder> {
    private ArrayList<String> labelNameList = new ArrayList<>();
    @NonNull
    @Override
    public Holder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        final ResultFilterLabelBinding resultFilterLabelBinding = DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.result_filter_label, parent,false);
        return new Holder(resultFilterLabelBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull SearchResultFilterLabelAdapter.Holder holder, int position) {
        if(position > labelNameList.size()) return;
        holder.binding.poiChargeLabel.setText(labelNameList.get(position));
    }

    @Override
    public int getItemCount() {
        Logger.d("huangli","getItemCount: "+labelNameList.size());
        return labelNameList.size();
    }

    /**
     * 设置label列表
     * @param list
     */
    public void setLabelNameList(final ArrayList<String> list) {
        if (ConvertUtils.isEmpty(list)) {
            return;
        }

        labelNameList.clear();
        labelNameList.addAll(list);
        notifyDataSetChanged();
    }

    public static class Holder extends RecyclerView.ViewHolder {
        private final ResultFilterLabelBinding binding;

        public Holder(@NonNull ResultFilterLabelBinding itemView) {
            super(itemView.getRoot());
            this.binding = itemView;
        }
    }
}
