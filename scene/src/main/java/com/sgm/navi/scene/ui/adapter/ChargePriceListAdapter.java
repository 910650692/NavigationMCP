package com.sgm.navi.scene.ui.adapter;

import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.ChargePriceItemBinding;
import com.sgm.navi.service.define.search.CostTime;

import java.util.ArrayList;

public class ChargePriceListAdapter extends RecyclerView.Adapter<ChargePriceListAdapter.priceHolder>{
    private ArrayList<CostTime> mChargePriceList = new ArrayList<>();
    @NonNull
    @Override
    public priceHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        ChargePriceItemBinding chargePriceItemBinding = DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.charge_price_item,parent,false);
        return new priceHolder(chargePriceItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull priceHolder holder, int position) {
        holder.chargePriceItemBinding.setCostTime(mChargePriceList.get(position));
    }

    // 更新列表
    public void notifyList(ArrayList<CostTime> list){
        mChargePriceList.addAll(list);
        notifyDataSetChanged();
    }

    @Override
    public int getItemCount() {
        if(mChargePriceList == null){
            return 0;
        }
        return mChargePriceList.size();
    }

    public static class priceHolder extends RecyclerView.ViewHolder{
        public ChargePriceItemBinding chargePriceItemBinding;
        public priceHolder(@NonNull ChargePriceItemBinding binding) {
            super(binding.getRoot());
            this.chargePriceItemBinding = binding;
        }
    }
}
